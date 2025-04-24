(*
 * Copyright (c) 2011-2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013      Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Memory allocation. *)

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 

type t = {
  buffer: buffer;
  off: int;
  len: int;
}
(** Type of memory blocks. *)

val page_size : int
(** Size of one page of memory in bytes. *)

val get_addr : t -> nativeint
(** [get_addr t] returns the address of the underlying bigarray of [t].
    This is useful for debugging *)

val get_page : t -> nativeint
(** [get_page t] returns the page offset (get_addr t) mod page_size, starting at 0 .*)

val get : int -> t
(** [get n] allocates and returns a memory block of [n] pages. If
    there is not enough memory, an [Out_of_memory] exception is
    raised. Note that this may be a recoverable situation, since
    this function allocates memory from a page-aligned pool, whereas
    the OCaml garbage collector will be running in its own heap that
    may have spare memory despite the [Out_of_memory] being raised
    from this function call. *)

exception Buffer_is_not_page_aligned
exception Buffer_not_multiple_of_page_size

val unsafe_of_bigarray : ?off:int -> ?len:int -> buffer -> t
val of_bigarray : ?off:int -> ?len:int -> buffer -> t

val unsafe_sub: t -> int -> int -> t
val sub: t -> int -> int -> t

val to_pages : t -> t list
(** [to_pages t] is a list of [size] memory blocks of one page each,
    where [size] is the size of [t] in pages. *)

val get_order : int -> t
(** [get_order i] is [get (1 lsl i)]. *)

val pages : int -> t list
(** [pages n] allocates a memory block of [n] pages and return the
    list of pages allocated. *)

val pages_order : int -> t list
(** [pages_order i] is [pages (1 lsl i)]. *)

val length : t -> int
(** [length t] is the size of [t], in bytes. *)

val to_string : t -> string
(** [to_string t] will allocate a fresh {!string} and copy the contents of [t]
    into the string. *)

val string_blit : string -> int -> t -> int -> int -> unit
(** [string_blit src srcoff dst dstoff len] copies [len] bytes from
    string [src], starting at byte number [srcoff], to memory block
    [dst], starting at byte number dstoff. *)

val blit_to_bytes : t -> int -> bytes -> int -> int -> unit

val blit: t -> int -> t -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters from
    cstruct [src], starting at index [srcoff], to cstruct [dst],
    starting at index [dstoff]. It works correctly even if [src] and
    [dst] are the same string, and the source and destination
    intervals overlap.

    @raise Invalid_argument if [srcoff] and [len] do not designate a
    valid segment of [src], or if [dstoff] and [len] do not designate
    a valid segment of [dst]. *)

val round_to_page_size : int -> int
(** [round_to_page_size n] returns the number of bytes that will be
    allocated for storing [n] bytes in memory *)

val shift: t -> int -> t
(** [shift cstr len] is [{ cstr with off=t.off+len; len=t.len-len }]
    @raise Invalid_argument if the offset exceeds cstruct length. *)

type uint8 = int
type uint16 = int
type uint32 = int32

val set_uint8: t -> int -> uint8 -> unit
val get_uint8: t -> int -> uint8
val set_le_uint16: t -> int -> uint16 -> unit
val get_le_uint16: t -> int -> uint16
val set_le_uint32: t -> int -> uint32 -> unit
val get_le_uint32: t -> int -> uint32

(* val shift: t -> int -> t *)
