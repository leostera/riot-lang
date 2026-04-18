open Prelude

module IoSlice = Iovec.IoSlice

type t = {
  slice: IoSlice.t;
  offset: int;
  len: int;
}

let empty = { slice = IoSlice.create ~size:0; offset = 0; len = 0 }

let of_slice = fun slice -> { slice; offset = 0; len = IoSlice.length slice }

let of_string = fun value ->
  let len = String.length value in
  let slice = IoSlice.create ~size:len in
  IoSlice.blit_from_string value ~src_offset:0 ~dst:slice ~dst_offset:0 ~len;
  { slice; offset = 0; len }

let of_buffer = fun buffer -> of_slice (Buffer.readable_slice buffer)

let length = fun view -> view.len

let get = fun view ~at ->
  if at < 0 || at >= view.len then
    System_error.panic "invalid string view index";
  IoSlice.get view.slice ~at:(view.offset + at)

let sub = fun view ~offset ~len ->
  if offset < 0 || len < 0 || offset + len > view.len then
    System_error.panic "invalid string view slice";
  { slice = view.slice; offset = view.offset + offset; len }

let advance = fun view ~by ->
  if by < 0 || by > view.len then
    System_error.panic "invalid string view advance";
  { slice = view.slice; offset = view.offset + by; len = view.len - by }

let starts_with = fun view ~prefix ->
  let prefix_len = String.length prefix in
  if prefix_len > view.len then
    false
  else
    let rec loop index =
      if index >= prefix_len then
        true
      else if IoSlice.get view.slice ~at:(view.offset + index) != String.get_unchecked prefix ~at:index then
        false
      else
        loop (index + 1)
    in
    loop 0

let index_of_char = fun view needle ->
  let rec loop index =
    if index >= view.len then
      None
    else if IoSlice.get view.slice ~at:(view.offset + index) = needle then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let index_of_string = fun view needle ->
  let needle_len = String.length needle in
  if needle_len = 0 then
    Some 0
  else
    let rec matches start needle_index =
      if needle_index >= needle_len then
        true
      else if IoSlice.get view.slice ~at:(view.offset + start + needle_index) != String.get_unchecked needle ~at:needle_index then
        false
      else
        matches start (needle_index + 1)
    in
    let rec loop index =
      if index + needle_len > view.len then
        None
      else if matches index 0 then
        Some index
      else
        loop (index + 1)
    in
    loop 0

let to_string = fun view -> IoSlice.to_string (IoSlice.sub view.slice ~offset:view.offset ~len:view.len)
