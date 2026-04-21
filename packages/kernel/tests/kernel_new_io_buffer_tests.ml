open Std
module Kernel = Kernel
module Test = Std.Test

let write_string = fun slice value ->
  let len = String.length value in
  Kernel.IO.IoVec.IoSlice.blit_from_string_unchecked value ~src_off:0 slice ~dst_off:0 ~len

let test_append_roundtrip = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () |> Result.unwrap in
  let suffix = Kernel.IO.IoVec.IoSlice.create ~size:4 |> Result.unwrap in
  write_string suffix "riot";
  let _ = Kernel.IO.Buffer.append_string buffer "hello" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_bytes buffer (Kernel.Bytes.from_string " ") |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_slice buffer suffix |> Result.unwrap in
  if String.equal (Kernel.IO.Buffer.to_string buffer) "hello riot" then
    Ok ()
  else
    Error "expected buffer append operations to preserve payload order"

let test_consume_compacts_existing_storage = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:4 () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "abcd" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.consume buffer ~len:2 |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "ef" |> Result.unwrap in
  if String.equal (Kernel.IO.Buffer.to_string buffer) "cdef" then
    Ok ()
  else
    Error "expected buffer compaction to preserve unread bytes"

let test_growth_preserves_contents = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:4 () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "abcd" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "efgh" |> Result.unwrap in
  if
    Kernel.IO.Buffer.capacity buffer >= 8 && String.equal (Kernel.IO.Buffer.to_string buffer) "abcdefgh"
  then
    Ok ()
  else
    Error "expected buffer growth to preserve readable contents"

let test_writable_slice_commit_exposes_written_bytes = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:8 () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.ensure_free buffer 5 |> Result.unwrap in
  let writable = Kernel.IO.Buffer.writable buffer in
  write_string writable "hello";
  let _ = Kernel.IO.Buffer.commit buffer 5 |> Result.unwrap in
  if String.equal (Kernel.IO.Buffer.to_string buffer) "hello" then
    Ok ()
  else
    Error "expected committed writable bytes to become readable"

let test_to_iovec_views_readable_slice = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = Kernel.IO.Buffer.to_iovec buffer |> Kernel.IO.IoVec.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected buffer iovec view to match readable region"

let tests = [
  Test.case "Buffer append operations preserve payload order" test_append_roundtrip;
  Test.case "Buffer compacts unread bytes before appending" test_consume_compacts_existing_storage;
  Test.case "Buffer growth preserves contents" test_growth_preserves_contents;
  Test.case "Buffer writable_slice becomes readable after commit" test_writable_slice_commit_exposes_written_bytes;
  Test.case "Buffer to_iovec views the readable region" test_to_iovec_views_readable_slice;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_io_buffer_tests" ~tests ~args ()

let () = Actors.run ~main ~args:Env.args ()
