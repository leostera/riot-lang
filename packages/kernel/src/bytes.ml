open Prelude

type t = bytes

type error =
  | OutOfBoundSet of { bytes: bytes; lenght: int; at: int; char: char }

let create = fun ~size -> Caml_runtime.bytes_create size

let length = Caml_runtime.bytes_length

let get = fun value ~at ->
  if at < 0 || at >= length value then
    None
  else Some (Caml_runtime.bytes_get value at)

let get_unchecked = fun value ~at -> Caml_runtime.bytes_get value at

let set_unchecked = fun value ~at ~char -> Caml_runtime.bytes_set value at char

let unsafe_set = fun value index char -> set_unchecked value ~at:index ~char

let set = fun value ~at ~char ->
  if at < 0 || at >= length value then
    Error (
      OutOfBoundSet {
        bytes = value;
        lenght = length value;
        at;
        char
      }
    )
  else Ok (set_unchecked value ~at ~char)

let blit = fun src ~src_offset ~dst ~dst_offset ~len ->
  let src_len = length src in
  let dst_len = length dst in
  if src_offset < 0 || len < 0 || src_offset + len > src_len then
    Error (
      OutOfBoundSet {
        bytes = src;
        lenght = src_len;
        at = src_offset;
        char = '\000'
      }
    )
  else
    if dst_offset < 0 || len < 0 || dst_offset + len > dst_len then
      Error (
        OutOfBoundSet {
          bytes = dst;
          lenght = dst_len;
          at = dst_offset;
          char = '\000'
        }
      )
    else Ok (Caml_runtime.bytes_blit src src_offset dst dst_offset len)

let blit_unchecked = fun src ~src_offset ~dst ~dst_offset ~len -> Caml_runtime.bytes_blit src src_offset dst dst_offset len

let fill = fun value ~offset ~len ~char -> Caml_runtime.bytes_fill value offset len char

let from_string = Caml_runtime.bytes_of_string

let to_string = Caml_runtime.bytes_to_string

let unsafe_to_string = Caml_runtime.bytes_unsafe_to_string

let sub_unchecked = fun value ~offset ~len ->
  let out = create ~size:len in
  Caml_runtime.bytes_blit value offset out 0 len;
  out

let sub = fun value ~offset ~len ->
  if offset < 0 || len < 0 || offset + len > length value then
    Error (
      OutOfBoundSet {
        bytes = value;
        lenght = length value;
        at = offset;
        char = '\000'
      }
    )
  else Ok (sub_unchecked value ~offset ~len)

let sub_string = fun value ~offset ~len ->
  match sub value ~offset ~len with
  | Ok slice -> to_string slice
  | Error _ -> System_error.panic "Kernel.Bytes.sub_string: out of bounds"
