(*
 * Copyright (c) 2019 Magnus Skjegstad <magnus@skjegstad.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* Vmnet unix example with port forwarding from an external interface. As we
 * don't have an OS to help us, we use Mirage libraries to construct a DHCP
 * request and send gARP to announce our IP to the bridge. The local IP is then
 * used to set up the firewall rule to forward a port from the external 
 * interface (typically en0).
 *
 * Note: In 10.15 it seems that the port will only be open if accessed from the
 * network. Connecting to the external IP from the same machine will not work.
 *)

let rec blocking_read t buf = 
        Vmnet.wait_for_event t;
        try
                Vmnet.read t buf
        with
        | Vmnet.No_packets_waiting -> blocking_read t buf
        | e -> raise e

let rec wait_for_lease vmnet_t state =
        let pkt_buf = (Cstruct.create (Vmnet.max_packet_size vmnet_t)) in
        match state with
        | None -> begin
              let random_xid = (Random.int32 Int32.max_int) in
              let mac = Vmnet.mac vmnet_t in
              print_endline (Printf.sprintf "Requesting DHCP lease for vmnet interface with mac %s" (Macaddr.to_string mac));
              let (dhcp_t, pkt) = Dhcp_client.create random_xid mac in
              Vmnet.write vmnet_t (Dhcp_wire.buf_of_pkt pkt);
              wait_for_lease vmnet_t (Some dhcp_t)
        end
        | Some dhcp_t -> begin
              let buf = blocking_read vmnet_t pkt_buf in
              match (Dhcp_client.input dhcp_t buf) with
              | `Noop -> begin
                      wait_for_lease vmnet_t (Some dhcp_t)
              end
              | `Response (dhcp_t', pkt) -> begin
                      Vmnet.write vmnet_t (Dhcp_wire.buf_of_pkt pkt);
                      wait_for_lease vmnet_t (Some dhcp_t')
              end
              | `New_lease (_, pkt') -> begin
                      pkt'.yiaddr
              end
        end

let send_garp vmnet_t mac ip =
  let garp = Arp_packet.({
          operation = Request;
          source_mac = mac;
          target_mac = (Macaddr.of_octets_exn (Cstruct.to_string (Cstruct.create 6))); (* all 0s *)
          source_ip = ip;
          target_ip = ip;
  })
  in
  let ether = Ethernet_packet.({
          source = mac;
          destination = Macaddr.broadcast;
          ethertype = `ARP
  }) in
  let arp_pkt = (Cstruct.create (Arp_packet.size + Ethernet_wire.sizeof_ethernet)) in
  let _ = Ethernet_packet.Marshal.into_cstruct ether arp_pkt in
  Arp_packet.encode_into garp (Cstruct.sub arp_pkt Ethernet_wire.sizeof_ethernet Arp_packet.size);
  Vmnet.write vmnet_t arp_pkt

let _ =
  Random.self_init ();

  (* init vmnet interface *)
  print_endline "Calling Vmnet.init ()";
  let vmnet_t = Vmnet.init ~mode:Shared_mode () in
  Printf.printf "Vmnet interface UUID is %s\n" (Uuidm.to_string (Vmnet.uuid vmnet_t));
  let () = Vmnet.set_event_handler vmnet_t in

  (* get DHCP lease *)
  let local_ip = (wait_for_lease vmnet_t None) in
  print_endline (Printf.sprintf "Got DHCP lease %s" (Ipaddr.V4.to_string local_ip));

  (* send gARP so gateway finds us when routing external traffic *)
  let mac = Vmnet.mac vmnet_t in
  print_endline (Printf.sprintf "Sending gARP for %s" (Macaddr.to_string mac));
  send_garp vmnet_t mac local_ip;

  (* set up port forwarding *)
  print_endline "Creating some forwarding rules...";
  Vmnet.add_port_forwarding_rule vmnet_t TCP 1234 local_ip 1234;
  Vmnet.add_port_forwarding_rule vmnet_t TCP 1235 local_ip 1235;
  Vmnet.add_port_forwarding_rule vmnet_t TCP 1236 local_ip 1236;
  Vmnet.add_port_forwarding_rule vmnet_t UDP 1234 local_ip 1234;

  print_endline "Removing one rule";
  Vmnet.remove_port_forwarding_rule vmnet_t UDP 1234;

  print_endline "Active firewall rules:";
  print_endline "proto\text\tinternal ip\tint";
  Array.iter (fun (proto, ext_port, ip, int_port) ->
        let proto_s =
                Vmnet.(match proto with
                | TCP -> "tcp"
                | UDP -> "udp"
                | ICMP -> "icmp"
                | Other _ -> "other")
        in
        Printf.printf "%s\t%d\t%s\t%d\n" proto_s ext_port (Ipaddr.V4.to_string ip) int_port)
        (Vmnet.get_port_forwarding_rules vmnet_t);

  print_endline "Traffic sent to the external IP on TCP port 1234 should now appear here.";

  (* output incoming packet headers *)
  let rec listen_and_print () =
          let buf = blocking_read vmnet_t (Cstruct.create (Vmnet.max_packet_size vmnet_t)) in
          (match Ethernet_wire.(int_to_ethertype (get_ethernet_ethertype buf)) with
          | Some `IPv4 -> begin
                (* manually extract ip src/dst *)
                let src_ip = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 buf (Ethernet_wire.sizeof_ethernet + 12)) in
                let dst_ip = Ipaddr.V4.of_int32 (Cstruct.BE.get_uint32 buf (Ethernet_wire.sizeof_ethernet + 16)) in
                let proto = Cstruct.get_uint8 buf (Ethernet_wire.sizeof_ethernet + 9) in
                (match (Ipaddr.V4.compare dst_ip local_ip), proto with
                | 0, 6 -> begin (* dst=local_ip, proto=tcp *)
                                print_endline (Printf.sprintf "src %s, dst %s, proto %d" (Ipaddr.V4.to_string src_ip) (Ipaddr.V4.to_string dst_ip) proto)
                        end 
                | _ -> ())
          end
          | Some `ARP -> begin 
                  (* reply to ARP requests if the initial gARP times out. This is a bit hacky as we send gARP in reply to requests *)
                  let arp_buf = Cstruct.sub buf Ethernet_wire.sizeof_ethernet ((Cstruct.len buf) - Ethernet_wire.sizeof_ethernet) in
                  (match (Arp_packet.decode arp_buf) with
                  | Error _ -> ()
                  | Ok arp_t -> begin
                          match (Ipaddr.V4.compare arp_t.target_ip local_ip) with
                          | 0 -> send_garp vmnet_t mac local_ip
                          | _ -> ()
                  end)
          end
          | _ -> ());
          listen_and_print ()
  in
  listen_and_print ()
