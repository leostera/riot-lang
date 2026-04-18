open Prelude

module IoSlice = Io_slice

type segment = IoSlice.t
type t = segment array

let create = fun ?(count = 1) ~size () ->
  if count <= 0 then
    System_error.panic "iovec count must be positive";
  if size < 0 then
    System_error.panic "iovec size must be non-negative";
  let base = size / count in
  let remainder = size mod count in
  Array.init ~count
    ~fn:(fun index ->
      let chunk =
        if index < remainder then
          base + 1
        else
          base
      in
      IoSlice.create ~size:chunk)

let with_capacity = fun size -> create ~size ()

let copy_bytes = fun source ->
  let len = Bytes.length source in
  let copy = IoSlice.create ~size:len in
  IoSlice.blit_from_bytes source ~src_offset:0 ~dst:copy ~dst_offset:0 ~len;
  copy

let copy_string = fun source ->
  let len = String.length source in
  let copy = IoSlice.create ~size:len in
  IoSlice.blit_from_string source ~src_offset:0 ~dst:copy ~dst_offset:0 ~len;
  copy

let from_bytes = fun buffer -> [|copy_bytes buffer|]

let from_string = fun value -> [|copy_string value|]

let from_bytes_array = fun buffers -> Array.map buffers ~fn:copy_bytes

let from_string_array = fun values -> Array.map values ~fn:copy_string

let length = fun segments ->
  Array.fold_left segments ~fn:(fun total segment -> total + IoSlice.length segment) ~acc:0

let for_each = fun ~fn segments -> Array.for_each segments ~fn

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
      Array.from_list (reverse_append acc [])
    else
      let segment = Array.get_unchecked segments ~at:index in
      let segment_length = IoSlice.length segment in
      let segment_start = cursor in
      let segment_end = cursor + segment_length in
      if segment_end <= pos then
        loop (index + 1) segment_end acc
      else
        let start_offset =
          if pos > segment_start then
            pos - segment_start
          else
            0
        in
        let available = segment_length - start_offset in
        let remaining = pos + len - (segment_start + start_offset) in
        let take =
          if available < remaining then
            available
          else
            remaining
        in
        let next = IoSlice.sub segment ~offset:start_offset ~len:take in
        loop (index + 1) segment_end (next :: acc)
  in
  loop 0 0 []

let to_bytes = fun segments ->
  let total = length segments in
  let out = Bytes.create ~size:total in
  let rec loop index cursor =
    if index >= Array.length segments then
      out
    else
      let segment = Array.get_unchecked segments ~at:index in
      IoSlice.blit_to_bytes segment ~dst:out ~dst_offset:cursor;
      loop (index + 1) (cursor + IoSlice.length segment)
  in
  let _ = loop 0 0 in
  out

let to_string = fun segments -> Bytes.to_string (to_bytes segments)
