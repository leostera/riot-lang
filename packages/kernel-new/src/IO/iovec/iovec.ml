open Prelude

type segment = {
  buffer: bytes;
  offset: int;
  length: int;
}

type t = segment array

let validate_bounds = fun ~offset ~length ~buffer ->
  if offset < 0 || length < 0 || offset + length > Bytes.length buffer then
    System_error.panic "invalid iovec bounds"

let make_segment = fun ~buffer ~offset ~length ->
  validate_bounds ~offset ~length ~buffer;
  { buffer; offset; length }

let create = fun ?(count = 1) ~size () ->
  if count <= 0 then
    System_error.panic "iovec count must be positive";
  if size < 0 then
    System_error.panic "iovec size must be non-negative";
  let base = size / count in
  let remainder = size mod count in
  Array.init count
    (fun index ->
      let chunk =
        if index < remainder then
          base + 1
        else
          base
      in
      { buffer = Bytes.create chunk; offset = 0; length = chunk })

let with_capacity = fun size -> create ~size ()

let of_bytes = fun buffer -> [|make_segment ~buffer ~offset:0 ~length:(Bytes.length buffer)|]

let of_string = fun value -> of_bytes (Bytes.of_string value)

let of_bytes_array = fun buffers ->
  Array.map (fun buffer -> make_segment ~buffer ~offset:0 ~length:(Bytes.length buffer)) buffers

let of_string_array = fun values -> of_bytes_array (Array.map Bytes.of_string values)

let length = fun segments ->
  Array.fold_left (fun total segment -> total + segment.length) 0 segments

let iter = fun fn segments ->
  Array.iter fn segments

let sub = fun ?(pos = 0) ~len segments ->
  if pos < 0 || len < 0 then
    System_error.panic "invalid iovec slice";
  let rec reverse_append left right =
    match left with
    | [] -> right
    | head :: tail -> reverse_append tail (head :: right)
  in
  let rec loop index cursor acc =
    if index >= Array.length segments || cursor >= pos + len then
      Array.of_list (reverse_append acc [])
    else
      let segment = Array.get segments index in
      let segment_start = cursor in
      let segment_end = cursor + segment.length in
      if segment_end <= pos then
        loop (index + 1) segment_end acc
      else
        let start_offset =
          if pos > segment_start then
            pos - segment_start
          else
            0
        in
        let available = segment.length - start_offset in
        let remaining = pos + len - (segment_start + start_offset) in
        let take =
          if available < remaining then
            available
          else
            remaining
        in
        let next = make_segment
          ~buffer:segment.buffer
          ~offset:((segment.offset + start_offset))
          ~length:take in
        loop (index + 1) segment_end (next :: acc)
  in
  loop 0 0 []

let into_bytes = fun segments ->
  let total = length segments in
  let out = Bytes.create total in
  let rec loop index cursor =
    if index >= Array.length segments then
      out
    else
      let segment = Array.get segments index in
      Bytes.blit segment.buffer segment.offset out cursor segment.length;
      loop (index + 1) (cursor + segment.length)
  in
  let _ = loop 0 0 in
  out

let into_string = fun segments -> Bytes.unsafe_to_string (into_bytes segments)
