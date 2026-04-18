open Std

module Test = Std.Test

let write_string = fun slice value ->
  let len = String.length value in
  IO.Iovec.IoSlice.blit_from_string value ~src_offset:0 ~dst:slice ~dst_offset:0 ~len

let test_iobuffer_append_roundtrip = fun _ctx ->
  let buffer = IO.IoBuffer.create () in
  IO.IoBuffer.append_string buffer "hello";
  IO.IoBuffer.append_bytes buffer (IO.Bytes.from_string " ");
  let writable = IO.IoBuffer.writable_slice ~size:4 buffer in
  write_string writable "riot";
  IO.IoBuffer.commit_write buffer ~len:4;
  if String.equal (IO.IoBuffer.to_string buffer) "hello riot" then
    Ok ()
  else
    Error "expected Std.IO.IoBuffer to preserve appended payloads"

let test_iobuffer_to_iovec_views_readable_region = fun _ctx ->
  let buffer = IO.IoBuffer.create () in
  IO.IoBuffer.append_string buffer "hello riot";
  IO.IoBuffer.consume buffer ~len:6;
  let actual = IO.IoBuffer.to_iovec buffer |> IO.Iovec.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected Std.IO.IoBuffer iovec view to match readable bytes"

let tests = [
  Test.case "Std.IO.IoBuffer append roundtrip" test_iobuffer_append_roundtrip;
  Test.case "Std.IO.IoBuffer to_iovec views the readable region" test_iobuffer_to_iovec_views_readable_region;
]

let main = fun ~args -> Test.Cli.main ~name:"std_io_iobuffer_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
