open Prelude

module IoSlice = Iovec.IoSlice

type t = {
  slice: IoSlice.t;
  offset: int;
  len: int;
}

type error = Error.t

let empty = { slice = IoSlice.empty; offset = 0; len = 0 }

let from_slice = fun slice -> { slice; offset = 0; len = IoSlice.length slice }

let from_string = fun value ->
  match IoSlice.from_string value with
  | Ok slice -> Ok (from_slice slice)
  | Error _ as error -> error

let from_buffer = fun buffer -> from_slice (Buffer.readable buffer)

let to_slice = fun view -> IoSlice.sub_unchecked view.slice ~off:view.offset ~len:view.len

let length = fun view -> view.len

let get = fun view ~at ->
  if at < 0 || at >= view.len then
    Error (Error.Index_out_of_bounds { buffer_length = view.len; at })
  else
    IoSlice.get view.slice ~at:(view.offset + at)

let get_unchecked = fun view ~at ->
  match get view ~at with
  | Ok char -> char
  | Error error -> System_error.panic ("Kernel.IO.StringView.get_unchecked: " ^ Error.message error)

let sub = fun view ~off ~len ->
  if off < 0 then
    Error (Error.Negative_offset off)
  else if len < 0 then
    Error (Error.Negative_length len)
  else if off > view.len || len > view.len - off then
    Error (Error.Range_out_of_bounds { buffer_length = view.len; offset = off; len })
  else
    Ok { slice = view.slice; offset = view.offset + off; len }

let shift = fun view by ->
  if by < 0 || by > view.len then
    Error (Error.Shift_out_of_bounds { buffer_length = view.len; by })
  else
    Ok { slice = view.slice; offset = view.offset + by; len = view.len - by }

let split_at = fun view at ->
  if at < 0 || at > view.len then
    Error (Error.Split_out_of_bounds { buffer_length = view.len; at })
  else
    Ok (
      { slice = view.slice; offset = view.offset; len = at },
      { slice = view.slice; offset = view.offset + at; len = view.len - at }
    )

let starts_with = fun view ~prefix ->
  let prefix_len = String.length prefix in
  if prefix_len > view.len then
    false
  else
    let rec loop index =
      if index >= prefix_len then
        true
      else if get_unchecked view ~at:index != String.get_unchecked prefix ~at:index then
        false
      else
        loop (index + 1)
    in
    loop 0

let equal_string = fun view string -> starts_with view ~prefix:string && view.len = String.length string

let index_char = fun view needle ->
  let rec loop index =
    if index >= view.len then
      None
    else if get_unchecked view ~at:index = needle then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let index_string = fun view needle ->
  let needle_len = String.length needle in
  if needle_len = 0 then
    Some 0
  else
    let rec matches start needle_index =
      if needle_index >= needle_len then
        true
      else if get_unchecked view ~at:(start + needle_index) != String.get_unchecked needle ~at:needle_index then
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

let to_string = fun view -> IoSlice.to_string (to_slice view)
