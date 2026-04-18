open Std

module Test = Std.Test

let test_string_view_search_helpers = fun _ctx ->
  let view = IO.StringView.from_string "GET /path HTTP/1.1\r\n\r\n" |> Result.unwrap in
  if
    IO.StringView.starts_with view ~prefix:"GET "
    && IO.StringView.index_char view ' ' = Some 3
    && IO.StringView.index_string view "\r\n\r\n" = Some 18
  then
    Ok ()
  else
    Error "expected Std.IO.StringView search helpers to find protocol delimiters"

let test_string_view_of_iobuffer_uses_readable_bytes = fun _ctx ->
  let buffer = IO.IoBuffer.create () |> Result.unwrap in
  let _ = IO.IoBuffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = IO.IoBuffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = IO.StringView.from_buffer buffer |> IO.StringView.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected Std.IO.StringView.from_buffer to use the readable region"

let tests = [
  Test.case "Std.IO.StringView finds HTTP delimiters" test_string_view_search_helpers;
  Test.case "Std.IO.StringView.from_buffer uses readable bytes" test_string_view_of_iobuffer_uses_readable_bytes;
]

let main = fun ~args -> Test.Cli.main ~name:"std_io_string_view_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
