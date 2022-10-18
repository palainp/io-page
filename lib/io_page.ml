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

type t = Bytes.t

type buf = Cstruct.t

let page_size = 1 lsl 12
let page_alignment = 4096

let length t = Bytes.length t

external alloc_pages: bool -> int -> t = "mirage_iopage_alloc_pages"

external c_get_addr : t -> nativeint = "mirage_iopage_get_addr"

let get_addr t = c_get_addr t

let get_page t = Nativeint.(div (get_addr t) (of_int page_size))

let get n =
  if n < 0
  then raise (Invalid_argument "Io_page.get cannot allocate a -ve number of pages")
  else (
    try alloc_pages false n with Out_of_memory ->
    Gc.compact ();
    alloc_pages true n
  )

let get_order order = get (1 lsl order)

let to_pages t =
  assert(length t mod page_size = 0);
  let rec loop off acc =
    if off < (length t)
    then loop (off + page_size) (
      let p = get 1 in
      Bytes.blit t off p 0 page_size ;
      p :: acc
    )
    else acc in
  List.rev (loop 0 [])

let pages n =
  let rec inner acc n =
    if n > 0 then inner ((get 1)::acc) (n-1) else acc
  in inner [] n

let pages_order order = pages (1 lsl order)

let round_to_page_size n = ((n + page_size - 1) lsr 12) lsl 12

let to_cstruct t = Cstruct.of_bytes t

exception Buffer_not_multiple_of_page_size

let of_cstruct_exn x =
  if ((Cstruct.length x) land (page_size-1) <> 0) then
    raise Buffer_not_multiple_of_page_size;
  let by = Cstruct.to_bytes x in
  let len = Bytes.length by in
  let n_pages = len / page_size in
  let align = get n_pages in
  Bytes.blit by 0 align 0 len ;
  align

let to_string t =
  Bytes.to_string t

let get_buf ?(n=1) () =
  to_cstruct (get n)

let blit src dest = Bytes.blit src 0 dest 0 (length src)

(* TODO: this is extremely inefficient.  Should use a ocp-endian
   blit rather than a byte-by-byte *)
let string_blit src srcoff dst dstoff len =
  let by = Bytes.of_string src in
  Bytes.blit by srcoff dst dstoff len
