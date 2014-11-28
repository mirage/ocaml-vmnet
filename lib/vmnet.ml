(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

type netif = {
  mtu: int;
  max_packet_size: int;
}

module Raw = struct
  type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  external init : int -> interface_ref = "caml_init_vmnet"
  external set_event_handler : interface_ref -> string -> unit = "caml_set_event_handler"
  external caml_vmnet_read : interface_ref -> buf -> int -> int -> int = "caml_vmnet_read"
  external caml_vmnet_write : interface_ref -> buf -> int -> int -> int = "caml_vmnet_write"

  cenum result {
    VMNET_SUCCESS = 1000;
    VMNET_FAILURE = 1001;
    VMNET_MEM_FAILURE = 1002;
    VMNET_INVALID_ARGUMENT = 1003;
    VMNET_SETUP_INCOMPLETE = 1004;
    VMNET_INVALID_ACCESS = 1005;
    VMNET_PACKET_TOO_BIG = 1006;
    VMNET_BUFFER_EXHAUSTED = 1007;
    VMNET_TOO_MANY_PACKETS = 1008
  } as uint32_t(sexp)
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
 | Unknown of int with sexp

exception Error of error with sexp

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

type mode =
  | Host_mode
  | Shared_mode

type t = {
  iface: interface_ref;
  name: string;
}

let iface_num = ref 0

let init ?(mode = Host_mode) () =
  let mode =
    match mode with
    | Host_mode -> 1000
    | Shared_mode -> 1001
  in
  let iface = Raw.init mode in
  let name = Printf.sprintf "vmnet%d" !iface_num in
  incr iface_num;
  { iface; name }

let set_event_handler {iface; name} f =
  Callback.register name f;
  Raw.set_event_handler iface name

let read {iface;_} c =
  Raw.caml_vmnet_read iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len
  |> function
  | len when len > 0 -> Cstruct.set_len c len
  | err -> raise (Error (error_of_int (err * (-1))))

let write {iface;_} c =
  Raw.caml_vmnet_write iface c.Cstruct.buffer c.Cstruct.off c.Cstruct.len
  |> function
  | len when len > 0 -> ()
  | err -> raise (Error (error_of_int (err * (-1))))
