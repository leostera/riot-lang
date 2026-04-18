open Std

module Kernel = Kernel
module Test = Std.Test

let write_string = fun slice value ->
  let len = String.length value in
  Kernel.IO.Iovec.IoSlice.blit_from_string value ~src_offset:0 ~dst:slice ~dst_offset:0 ~len

let test_append_roundtrip = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () in
  let suffix = Kernel.IO.Iovec.IoSlice.create ~size:4 in
  write_string suffix "riot";
  Kernel.IO.Buffer.append_string buffer "hello";
  Kernel.IO.Buffer.append_bytes buffer (Kernel.Bytes.from_string " ");
  Kernel.IO.Buffer.append_slice buffer suffix;
  if String.equal (Kernel.IO.Buffer.to_string buffer) "hello riot" then
    Ok ()
  else
    Error "expected buffer append operations to preserve payload order"

let test_consume_compacts_existing_storage = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:4 () in
  Kernel.IO.Buffer.append_string buffer "abcd";
  Kernel.IO.Buffer.consume buffer ~len:2;
  Kernel.IO.Buffer.append_string buffer "ef";
  if String.equal (Kernel.IO.Buffer.to_string buffer) "cdef" then
    Ok ()
  else
    Error "expected buffer compaction to preserve unread bytes"

let test_growth_preserves_contents = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:4 () in
  Kernel.IO.Buffer.append_string buffer "abcd";
  Kernel.IO.Buffer.append_string buffer "efgh";
  if Kernel.IO.Buffer.capacity buffer >= 8 && String.equal (Kernel.IO.Buffer.to_string buffer) "abcdefgh" then
    Ok ()
  else
    Error "expected buffer growth to preserve readable contents"

let test_writable_slice_commit_exposes_written_bytes = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create ~size:8 () in
  let writable = Kernel.IO.Buffer.writable_slice ~size:5 buffer in
  write_string writable "hello";
  Kernel.IO.Buffer.commit_write buffer ~len:5;
  if String.equal (Kernel.IO.Buffer.to_string buffer) "hello" then
    Ok ()
  else
    Error "expected committed writable bytes to become readable"

let test_to_iovec_views_readable_slice = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () in
  Kernel.IO.Buffer.append_string buffer "hello riot";
  Kernel.IO.Buffer.consume buffer ~len:6;
  let actual = Kernel.IO.Buffer.to_iovec buffer |> Kernel.IO.Iovec.to_string in
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

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_io_buffer_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
