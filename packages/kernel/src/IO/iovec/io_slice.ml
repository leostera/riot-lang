open Prelude

type t

external unsafe_create: int -> t = "kernel_new_iovec_slice_create"

let create = fun ~size ->
  if size < 0 then
    System_error.panic "invalid ioslice size";
  unsafe_create size

external length: t -> int = "%caml_ba_dim_1"

external unsafe_get: t -> int -> char = "%caml_ba_unsafe_ref_1"

external unsafe_set: t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external sub: t -> offset:int -> len:int -> t = "caml_ba_sub"

let blit_from_bytes = fun src ~src_offset ~dst ~dst_offset ~len ->
  let rec loop index =
    if index >= len then
      ()
    else (
      unsafe_set dst (dst_offset + index) (Caml_runtime.bytes_get src (src_offset + index));
      loop (index + 1)
    )
  in
  loop 0

let blit_to_bytes = fun src ~dst ~dst_offset ->
  let len = length src in
  let rec loop index =
    if index >= len then
      ()
    else (
      Caml_runtime.bytes_set dst (dst_offset + index) (unsafe_get src index);
      loop (index + 1)
    )
  in
  loop 0

let blit_from_string = fun src ~src_offset ~dst ~dst_offset ~len ->
  let rec loop index =
    if index >= len then
      ()
    else (
      unsafe_set dst (dst_offset + index) (Caml_runtime.string_get src (src_offset + index));
      loop (index + 1)
    )
  in
  loop 0

let to_string = fun value ->
  let out = Caml_runtime.bytes_create (length value) in
  blit_to_bytes value ~dst:out ~dst_offset:0;
  Caml_runtime.bytes_unsafe_to_string out
