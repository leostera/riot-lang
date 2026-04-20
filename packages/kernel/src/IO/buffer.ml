open Prelude

module IoSlice = IoVec.IoSlice

type t = {
  mutable storage: IoSlice.t;
  mutable start: int;
  mutable len: int;
}

type error = Error.t

let create = fun ?(size = 0) () ->
  match IoSlice.create ~size with
  | Ok storage -> Ok { storage; start = 0; len = 0 }
  | Error _ as error -> error

let length = fun buffer -> buffer.len

let readable_bytes = fun buffer -> buffer.len

let capacity = fun buffer -> IoSlice.length buffer.storage

let writable_bytes = fun buffer -> capacity buffer - (buffer.start + buffer.len)

let clear = fun buffer ->
  buffer.start <- 0;
  buffer.len <- 0

let compact = fun buffer ->
  if buffer.start = 0 then
    ()
  else if buffer.len = 0 then
    clear buffer
  else (
    IoSlice.blit_unchecked
      ~src:buffer.storage
      ~src_off:buffer.start
      ~dst:buffer.storage
      ~dst_off:0
      ~len:buffer.len;
    buffer.start <- 0
  )

let ensure_free = fun buffer needed ->
  if needed < 0 then
    Error (Error.Negative_length needed)
  else if writable_bytes buffer >= needed then
    Ok ()
  else if capacity buffer - buffer.len >= needed then (
    compact buffer;
    Ok ()
  ) else (
  let current_capacity = capacity buffer in
    let min_capacity = buffer.len + needed in
    let grown =
      if current_capacity = 0 then
        min_capacity
      else if current_capacity * 2 > min_capacity then
        current_capacity * 2
      else
        min_capacity
    in
    match IoSlice.create ~size:grown with
    | Error _ as error -> error
    | Ok next ->
        IoSlice.blit_unchecked ~src:buffer.storage ~src_off:buffer.start ~dst:next ~dst_off:0 ~len:buffer.len;
        buffer.storage <- next;
        buffer.start <- 0;
        Ok ()
  )

let readable = fun buffer ->
  IoSlice.sub_unchecked buffer.storage ~off:buffer.start ~len:buffer.len

let writable = fun buffer ->
  IoSlice.sub_unchecked buffer.storage ~off:(buffer.start + buffer.len) ~len:(writable_bytes buffer)

let commit = fun buffer requested ->
  if requested < 0 then
    Error (Error.Negative_length requested)
  else if requested > writable_bytes buffer then
    Error (Error.Commit_out_of_bounds { writable_bytes = writable_bytes buffer; requested })
  else (
    buffer.len <- buffer.len + requested;
    Ok ()
  )

let append_string = fun buffer value ->
  let len = String.length value in
  match ensure_free buffer len with
  | Error _ as error -> error
  | Ok () ->
      let dst = writable buffer in
      IoSlice.blit_from_string value ~src_off:0 dst ~dst_off:0 ~len
      |> Result.and_then ~fn:(fun () -> commit buffer len)

let append_bytes = fun buffer value ->
  let len = Bytes.length value in
  match ensure_free buffer len with
  | Error _ as error -> error
  | Ok () ->
      let dst = writable buffer in
      IoSlice.blit_from_bytes value ~src_off:0 dst ~dst_off:0 ~len
      |> Result.and_then ~fn:(fun () -> commit buffer len)

let append_slice = fun buffer value ->
  let len = IoSlice.length value in
  match ensure_free buffer len with
  | Error _ as error -> error
  | Ok () ->
      let dst = writable buffer in
      IoSlice.blit ~src:value ~src_off:0 ~dst ~dst_off:0 ~len
      |> Result.and_then ~fn:(fun () -> commit buffer len)

let consume = fun buffer ~len ->
  if len < 0 then
    Error (Error.Negative_length len)
  else if len > buffer.len then
    Error (Error.Consume_out_of_bounds { readable_bytes = buffer.len; requested = len })
  else (
    buffer.start <- buffer.start + len;
    buffer.len <- buffer.len - len;
    if buffer.len = 0 then
      buffer.start <- 0;
    Ok ()
  )

let to_iovec = fun buffer -> IoVec.from_slices [|readable buffer|]

let to_bytes = fun buffer ->
  let out = Bytes.create ~size:buffer.len in
  IoSlice.blit_to_bytes_unchecked (readable buffer) ~src_off:0 out ~dst_off:0 ~len:buffer.len;
  out

let to_string = fun buffer -> Bytes.to_string (to_bytes buffer)
