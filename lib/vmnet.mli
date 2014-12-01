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

(** MacOS X userspace network bridging *)

(** *)
type t with sexp_of

(** *)
type mode =
  | Host_mode
  | Shared_mode with sexp

(** *)
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

(** *)
exception Error of error with sexp

(** *)
exception No_packets_waiting with sexp

(** *)
val init : ?mode:mode -> unit -> t

(** *)
val mac : t -> Macaddr.t

(** *)
val max_packet_size: t -> int

(** *)
val set_event_handler : t -> unit

(** *)
val wait_for_event : t -> unit

(** *)
val read : t -> Cstruct.t -> Cstruct.t

(** *)
val write : t -> Cstruct.t -> unit

