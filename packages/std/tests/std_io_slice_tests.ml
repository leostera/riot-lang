open Std
module Test = Std.Test

let test_io_slice_search_helpers = fun _ctx ->
  let slice = IO.IoVec.IoSlice.from_string "GET /path HTTP/1.1\r\n\r\n" |> Result.unwrap in
  if
    IO.IoVec.IoSlice.starts_with slice ~prefix:"GET "
    && IO.IoVec.IoSlice.index_char slice ' ' = Some 3
    && IO.IoVec.IoSlice.index_string slice "\r\n\r\n" = Some 18
  then
    Ok ()
  else
    Error "expected Std.IO.IoSlice search helpers to find protocol delimiters"

let test_iobuffer_readable_uses_readable_bytes = fun _ctx ->
  let buffer = IO.IoBuffer.create () |> Result.unwrap in
  let _ = IO.IoBuffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = IO.IoBuffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = IO.IoBuffer.readable buffer |> IO.IoVec.IoSlice.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected Std.IO.IoSlice.to_string over readable bytes to preserve the readable region"

let tests = [
  Test.case "Std.IO.IoSlice finds HTTP delimiters" test_io_slice_search_helpers;
  Test.case "Std.IO.IoBuffer.readable exposes readable bytes" test_iobuffer_readable_uses_readable_bytes;
]

let main = fun ~args -> Test.Cli.main ~name:"std_io_slice_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
