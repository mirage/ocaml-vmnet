(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

open Sexplib.Conv

type interface_ref

module Raw = struct
  type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = {
    iface: interface_ref;
    mac: string;
    mtu: int;
    max_packet_size: int;
    uuid : string;
  }

  external init : int -> string -> string -> (string * string * string) option -> t = "caml_init_vmnet"
  external set_event_handler : interface_ref -> unit = "caml_set_event_handler"
  external wait_for_event : interface_ref -> unit = "caml_wait_for_event"
  external caml_vmnet_read : interface_ref -> buf -> int -> int -> int = "caml_vmnet_read"
  external caml_vmnet_write : interface_ref -> buf -> int -> int -> int = "caml_vmnet_write"
  external caml_shared_interface_list : unit -> string array = "caml_shared_interface_list"
  external caml_vmnet_interface_add_port_forwarding_rule : interface_ref -> int -> int -> string -> int -> int = "caml_vmnet_interface_add_port_forwarding_rule"
  external caml_vmnet_interface_remove_port_forwarding_rule : interface_ref -> int -> int -> int = "caml_vmnet_interface_remove_port_forwarding_rule"
  external caml_interface_get_port_forwarding_rules : interface_ref -> (int * int * string * int) array = "caml_interface_get_port_forwarding_rules"

  exception Return_code of int
  exception API_not_supported
  let _ = Callback.register_exception "vmnet_raw_return" (Return_code 0)
  let _ = Callback.register_exception "vmnet_api_not_supported" (API_not_supported)
end

type error =
 | Failure
 | Mem_failure
 | Invalid_argument
 | Setup_incomplete
 | Invalid_access
 | Packet_too_big
 | Buffer_exhausted
 | Too_many_packets
 | Unknown of int [@@deriving sexp]

exception Error of error [@@deriving sexp]
exception Permission_denied
exception No_packets_waiting [@@deriving sexp]

(* Possible values of vmnet_return_t *)
let error_of_int =
  function
  | 1001 -> Failure
  | 1002 -> Mem_failure
  | 1003 -> Invalid_argument
  | 1004 -> Setup_incomplete
  | 1005 -> Invalid_access
  | 1006 -> Packet_too_big
  | 1007 -> Buffer_exhausted
  | 1008 -> Too_many_packets
  | err  -> Unknown err

type ipv4_config = {
    ipv4_start_address: Ipaddr_sexp.V4.t;
    ipv4_end_address: Ipaddr_sexp.V4.t;
    ipv4_netmask: Ipaddr_sexp.V4.t;
} [@@deriving sexp]

type mode =
  | Host_mode
  | Shared_mode
  | Bridged_mode of string [@@deriving sexp]

type proto =
  | TCP
  | UDP
  | ICMP
  | Other of int [@@deriving sexp]

let int_of_proto =
  function
  | TCP     -> 6
  | UDP     -> 17
  | ICMP    -> 1
  | Other x -> x

let proto_of_int =
  function
  | 6  -> TCP
  | 17 -> UDP
  | 1  -> ICMP
  | x  -> Other x

type t = {
  iface: interface_ref sexp_opaque;
  name: string;
  mtu: int;
  mac: Macaddr_sexp.t;
  max_packet_size: int;
  uuid: Uuidm.t sexp_opaque;
} [@@deriving sexp_of]

let mac {mac; _} = mac
let mtu {mtu; _} = mtu
let max_packet_size {max_packet_size; _} = max_packet_size
let uuid {uuid; _} = uuid

let iface_num = ref 0

let init ?(mode = Shared_mode) ?(uuid = Uuidm.nil) ?ipv4_config () =
  let mode, iface =
    match mode with
    | Host_mode -> (1000, "")
    | Shared_mode -> (1001, "")
    | Bridged_mode iface -> (1002, iface)
  in
  let ip_to_str ip = Ipaddr.V4.to_string ip in
  let ipv4_config_str =
    match ipv4_config with
    | Some x -> Some ((ip_to_str x.ipv4_start_address),
                      (ip_to_str x.ipv4_end_address),
                      (ip_to_str x.ipv4_netmask))
    | None -> None
  in
  try
    let t = Raw.init mode iface (Uuidm.to_bytes uuid) ipv4_config_str
    in
    let name = Printf.sprintf "vmnet%d" !iface_num in
    incr iface_num;
    let mac = Macaddr.of_octets_exn t.Raw.mac in
    let mtu = t.Raw.mtu in
    let max_packet_size = t.Raw.max_packet_size in
    let uuid = (match Uuidm.of_bytes t.uuid with
      | None -> Uuidm.nil (* TODO: This shouldn't happen and could raise an error *)
      | Some x -> x) in
    { iface=t.Raw.iface; mac; mtu; max_packet_size; name; uuid }
  with
    | Raw.Return_code r -> if r = 1001 && Unix.geteuid() <> 0
			   then raise Permission_denied
			   else raise (Error (error_of_int r))

let set_event_handler {iface; _} =
  Raw.set_event_handler iface

let wait_for_event {iface; _} =
  Raw.wait_for_event iface

let read {iface;_} c =
  let r = Raw.caml_vmnet_read iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len in
  match r with
  | 0 -> raise No_packets_waiting
  | len when len > 0 -> Cstruct.sub c 0 len
  | err -> raise (Error (error_of_int (err * (-1))))

let write {iface;_} c =
  Raw.caml_vmnet_write iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len
  |> function
  | len when len > 0 -> ()
  | err -> raise (Error (error_of_int (err * (-1))))

let shared_interface_list =
  Raw.caml_shared_interface_list

let get_port_forwarding_rules {iface;_} =
  let f (proto, ext_port, int_addr, int_port) =
    (proto_of_int proto, ext_port, Ipaddr.V4.of_string_exn int_addr, int_port)
  in
  Array.map f (Raw.caml_interface_get_port_forwarding_rules iface)

let add_port_forwarding_rule {iface;_} protocol ext_port ip int_port =
  Raw.caml_vmnet_interface_add_port_forwarding_rule iface (int_of_proto protocol) ext_port (Ipaddr.V4.to_string ip) int_port
  |> function
  | 1000 -> () (* VMNET_SUCCESS *)
  | err -> raise (Error (error_of_int err))

let remove_port_forwarding_rule {iface;_} protocol ext_port =
  Raw.caml_vmnet_interface_remove_port_forwarding_rule iface (int_of_proto protocol) ext_port
  |> function
  | 1000 -> () (* VMNET_SUCCESS *)
  | err -> raise (Error (error_of_int err))
