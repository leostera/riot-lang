open Std
module Test = Std.Test

let write_string = fun slice value ->
  let len = String.length value in
  IO.IoVec.IoSlice.blit_from_string_unchecked value ~src_off:0 slice ~dst_off:0 ~len

let test_iobuffer_append_roundtrip = fun _ctx ->
  let buffer = IO.IoBuffer.create () |> Result.unwrap in
  let _ = IO.IoBuffer.append_string buffer "hello" |> Result.unwrap in
  let _ = IO.IoBuffer.append_bytes buffer (IO.Bytes.from_string " ") |> Result.unwrap in
  let _ = IO.IoBuffer.ensure_free buffer 4 |> Result.unwrap in
  let writable = IO.IoBuffer.writable buffer in
  write_string writable "riot";
  let _ = IO.IoBuffer.commit buffer 4 |> Result.unwrap in
  if String.equal (IO.IoBuffer.to_string buffer) "hello riot" then
    Ok ()
  else
    Error "expected Std.IO.IoBuffer to preserve appended payloads"

let test_iobuffer_to_iovec_views_readable_region = fun _ctx ->
  let buffer = IO.IoBuffer.create () |> Result.unwrap in
  let _ = IO.IoBuffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = IO.IoBuffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = IO.IoBuffer.to_iovec buffer |> IO.IoVec.to_string in
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
