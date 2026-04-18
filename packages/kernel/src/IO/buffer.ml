open Prelude

module IoSlice = Iovec.IoSlice

type t = {
  mutable storage: IoSlice.t;
  mutable start: int;
  mutable len: int;
}

let create = fun ?(size = 0) () ->
  if size < 0 then
    System_error.panic "invalid io buffer size";
  { storage = IoSlice.create ~size; start = 0; len = 0 }

let length = fun buffer -> buffer.len

let capacity = fun buffer -> IoSlice.length buffer.storage

let clear = fun buffer ->
  buffer.start <- 0;
  buffer.len <- 0

let ensure_writable = fun buffer ~needed ->
  if needed < 0 then
    System_error.panic "invalid io buffer writable length";
  let current_capacity = capacity buffer in
  let write_start = buffer.start + buffer.len in
  if current_capacity - write_start >= needed then
    ()
  else if current_capacity - buffer.len >= needed then (
    IoSlice.blit
      ~src:buffer.storage
      ~src_offset:buffer.start
      ~dst:buffer.storage
      ~dst_offset:0
      ~len:buffer.len;
    buffer.start <- 0
  ) else (
    let min_capacity = buffer.len + needed in
    let grown =
      if current_capacity = 0 then
        min_capacity
      else if current_capacity * 2 > min_capacity then
        current_capacity * 2
      else
        min_capacity
    in
    let next = IoSlice.create ~size:grown in
    IoSlice.blit ~src:buffer.storage ~src_offset:buffer.start ~dst:next ~dst_offset:0 ~len:buffer.len;
    buffer.storage <- next;
    buffer.start <- 0
  )

let writable_slice = fun ?(size = 1) buffer ->
  ensure_writable buffer ~needed:size;
  IoSlice.sub buffer.storage ~offset:(buffer.start + buffer.len) ~len:(capacity buffer - (buffer.start + buffer.len))

let commit_write = fun buffer ~len ->
  if len < 0 then
    System_error.panic "invalid io buffer commit length";
  let write_start = buffer.start + buffer.len in
  let writable = capacity buffer - write_start in
  if len > writable then
    System_error.panic "io buffer commit exceeds writable capacity";
  buffer.len <- buffer.len + len

let append_string = fun buffer value ->
  let len = String.length value in
  let dst = writable_slice ~size:len buffer in
  IoSlice.blit_from_string value ~src_offset:0 ~dst ~dst_offset:0 ~len;
  commit_write buffer ~len

let append_bytes = fun buffer value ->
  let len = Bytes.length value in
  let dst = writable_slice ~size:len buffer in
  IoSlice.blit_from_bytes value ~src_offset:0 ~dst ~dst_offset:0 ~len;
  commit_write buffer ~len

let append_slice = fun buffer value ->
  let len = IoSlice.length value in
  let dst = writable_slice ~size:len buffer in
  IoSlice.blit ~src:value ~src_offset:0 ~dst ~dst_offset:0 ~len;
  commit_write buffer ~len

let consume = fun buffer ~len ->
  if len < 0 || len > buffer.len then
    System_error.panic "invalid io buffer consume length";
  buffer.start <- buffer.start + len;
  buffer.len <- buffer.len - len;
  if buffer.len = 0 then
    buffer.start <- 0

let readable_slice = fun buffer -> IoSlice.sub buffer.storage ~offset:buffer.start ~len:buffer.len

let to_iovec = fun buffer -> Iovec.from_slices [|readable_slice buffer|]

let to_bytes = fun buffer ->
  let out = Bytes.create ~size:buffer.len in
  IoSlice.blit_to_bytes (readable_slice buffer) ~dst:out ~dst_offset:0;
  out

let to_string = fun buffer -> Bytes.to_string (to_bytes buffer)
