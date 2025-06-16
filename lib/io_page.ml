(*
 * Copyright (c) 2011-2012 Anil Madhavapeddy <anil@recoil.org>
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

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 

type t = {
  buffer: buffer;
  off: int;
  len: int;
}

let page_size = 1 lsl 12 (* 4096 *)
(* Alignement is now constrained by the allocation call.
   For pages created with of_bigarray and sub, the alignment constraint
   and the size of the buffer are checked. *)
let page_alignment = 4096

let length b = b.len

external alloc_pages: bool -> int -> buffer = "mirage_iopage_alloc_pages"

external c_get_addr : buffer -> nativeint = "mirage_iopage_get_addr"

let get_addr t = Nativeint.(add (c_get_addr t.buffer) (of_int t.off))

let get_page t = Nativeint.(div (get_addr t) (of_int page_size))

external check_alignment_bigstring : buffer -> int -> int -> bool = "mirage_iopage_check_alignment_bigstring"

exception Buffer_is_not_page_aligned
exception Buffer_not_multiple_of_page_size

let assert_alignement_and_size ba =
  if not(check_alignment_bigstring ba 0 page_alignment) then
    raise Buffer_is_not_page_aligned;
  let dim = Bigarray.Array1.dim ba in
  if dim land (page_size - 1) <> 0 then raise Buffer_not_multiple_of_page_size;
  ()

let get n =
  if n < 0
  then raise (Invalid_argument "Io_page.get cannot allocate a -ve number of pages")
  else (
    try
      {buffer=alloc_pages false n; off=0; len=n*page_size}
    with Out_of_memory ->
    Gc.compact ();
    {buffer=alloc_pages true n; off=0; len=n*page_size}
  )

let unsafe_of_bigarray ?(off=0) ?len ba =
  let dim = Bigarray.Array1.dim ba in
  let len =
    match len with
    | None     -> dim - off
    | Some len -> len in
  if off < 0 || len < 0 || off + len < 0 || off + len > dim then invalid_arg "of_bigarray"
  else {buffer=ba; off; len}

let of_bigarray ?(off=0) ?len ba =
  assert_alignement_and_size ba ;
  unsafe_of_bigarray ~off ?len ba

let unsafe_sub t off len =
  (* from https://github.com/mirage/ocaml-cstruct/pull/245

     Cstruct.sub should select what a programmer intuitively expects a
     sub-cstruct to be. I imagine holding out my hands, with the left
     representing the start offset and the right the end. I think of a
     sub-cstruct as any span within this range. If I move my left hand only to
     the right (new_start >= t.off), and my right hand only to the left
     (new_end <= old_end), and they don't cross (new_start <= new_end), then I
     feel sure the result will be a valid sub-cstruct. And if I violate any one
     of these constraints (e.g. moving my left hand further left), then I feel
     sure that the result wouldn't be something I'd consider to be a sub-cstruct.

     Wrapping considerations in modular arithmetic:

     Note that if x is non-negative, and x + y wraps, then x + y must be
     negative. This is easy to see with modular arithmetic because if y is
     negative then the two arguments will cancel to some degree the result
     cannot be further from zero than one of the arguments. If y is positive
     then x + y can wrap, but even max_int + max_int doesn't wrap all the way to
     zero.

     The three possibly-wrapping operations are:

     new_start = t.off + off. t.off is non-negative so if this wraps then
     new_start will be negative and will fail the new_start >= t.off test.

     new_end = new_start + len. The above test ensures that new_start is
     non-negative in any successful return. So if this wraps then new_end will
     be negative and will fail the new_start <= new_end test.

     old_end = t.off + t.len. This uses only the existing trusted values. It
     could only wrap if the underlying bigarray had a negative length!  *)
  let new_start = t.off + off in
  let new_end = new_start + len in
  let old_end = t.off + t.len in
  if new_start >= t.off && new_end <= old_end && new_start <= new_end then
    { t with off = new_start ; len }
  else
    invalid_arg ("sub fails: "^string_of_int(new_start)^" >= "^string_of_int(t.off)^" && "^string_of_int(new_end)^" <= "^string_of_int(old_end)^" && "^string_of_int(new_start)^" <= "^string_of_int(new_end))

let sub t off len =
  assert_alignement_and_size t.buffer ;
  unsafe_sub t off len

let unsafe_to_pages t =
  let rec loop off acc =
    if off < (length t)
    then loop (off + page_size) (sub t off page_size :: acc)
    else acc in
  List.rev (loop 0 [])

let to_pages t =
  assert_alignement_and_size t.buffer ;
  unsafe_to_pages t

let get_order order = get (1 lsl order)

let pages n =
  let rec inner acc n =
    if n > 0 then inner (get 1::acc) (n-1) else acc
  in inner [] n

let pages_order order = pages (1 lsl order)

let round_to_page_size n = ((n + page_size - 1) lsr 12) lsl 12

external unsafe_blit_bigstring_to_bigstring : buffer -> int -> buffer -> int -> int -> unit = "mirage_iopage_blit_bigstring_to_bigstring" [@@noalloc]
let blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    invalid_arg "blit with source indexes"
  else if dstoff < 0 || dst.len - dstoff < len then
    invalid_arg "blit with dest indexes"
  else
    unsafe_blit_bigstring_to_bigstring src.buffer (src.off+srcoff) dst.buffer
      (dst.off+dstoff) len

external unsafe_blit_string_to_bigstring : string -> int -> buffer -> int -> int -> unit = "mirage_iopage_blit_string_to_bigstring" [@@noalloc]
external unsafe_blit_bigstring_to_bytes : buffer -> int -> Bytes.t -> int -> int -> unit = "mirage_iopage_blit_bigstring_to_string" [@@noalloc]

let string_blit src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || dstoff < 0 || String.length src - srcoff < len then
    invalid_arg "blit with source indexes"
  else if dst.len - dstoff < len then
    invalid_arg "blit with dest indexes"
  else
    unsafe_blit_string_to_bigstring src srcoff dst.buffer (dst.off+dstoff) len

let blit_to_bytes src srcoff dst dstoff len =
  if len < 0 || srcoff < 0 || src.len - srcoff < len then
    invalid_arg "blit with source indexes"
  else if (Bytes.length dst) - dstoff < len then
    invalid_arg "blit with dest indexes"
  else
    unsafe_blit_bigstring_to_bytes src.buffer (src.off+srcoff) dst dstoff len

let to_string t =
  let len = length t in
  let dst = Bytes.create len in
  unsafe_blit_bigstring_to_bytes t.buffer t.off dst 0 len ;
  Bytes.unsafe_to_string dst

let check_bounds t len =
  len >= 0 && Bigarray.Array1.dim t.buffer >= len

let shift t amount =
  let off = t.off + amount in
  let len = t.len - amount in
  if amount < 0 || amount > t.len || not (check_bounds t (off+len)) then
    invalid_arg "shift amount"
  else { t with off; len }


type uint8 = int
type uint16 = int
type uint32 = int32

let set_uint8 t i c =
  if i >= (length t) || i < 0 then invalid_arg "set_uint8 invalid bound"
  else Bigarray.Array1.set t.buffer i (Char.unsafe_chr c)

let get_uint8 t i =
  if i >= (length t) || i < 0 then invalid_arg "get_uint8 invalid bound"
  else Char.code (Bigarray.Array1.get t.buffer i)

external ba_set_int16 : buffer -> int -> uint16 -> unit = "%caml_bigstring_set16u"
external ba_get_int16 : buffer -> int -> uint16 = "%caml_bigstring_get16u"
external swap16 : int -> int = "%bswap16"

let swap = Sys.big_endian

let set_le_uint16 t i c =
  if i > (length t) - 2 || i < 0 then invalid_arg "set_le_uint16 invalid bound"
  else ba_set_int16 t.buffer (t.off+i) (if swap then swap16 c else c) [@@inline]

let get_le_uint16 t i =
  if i > (length t) - 2 || i < 0 then invalid_arg "get_le_uint16 invalid bound"
  else
    let r = ba_get_int16 t.buffer (t.off+i) in
    if swap then swap16 r else r [@@inline]

external ba_set_int32 : buffer -> int -> uint32 -> unit = "%caml_bigstring_set32u"
external ba_get_int32 : buffer -> int -> uint32 = "%caml_bigstring_get32u"
external swap32 : int32 -> int32 = "%bswap_int32"

let set_le_uint32 t i c =
  if i > (length t) - 4 || i < 0 then invalid_arg "set_le_uint32 invalid bound"
  else ba_set_int32 t.buffer (t.off+i) (if swap then swap32 c else c) [@@inline]

let get_le_uint32 t i =
  if i > (length t) - 4 || i < 0 then invalid_arg "get_le_uint32 invalid bound"
  else
    let r = ba_get_int32 t.buffer (t.off+i) in
    if swap then swap32 r else r [@@inline]
