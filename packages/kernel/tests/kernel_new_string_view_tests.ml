open Std

module Kernel = Kernel
module Test = Std.Test

let test_of_string_roundtrip = fun _ctx ->
  let view = Kernel.IO.StringView.from_string "hello riot" |> Result.unwrap in
  if
    Kernel.IO.StringView.length view = 10
    && Kernel.IO.StringView.get view ~at:4 = Ok 'o'
    && String.equal (Kernel.IO.StringView.to_string view) "hello riot"
  then
    Ok ()
  else
    Error "expected string view to preserve string contents"

let test_sub_and_advance = fun _ctx ->
  let view = Kernel.IO.StringView.from_string "hello riot" |> Result.unwrap in
  let actual =
    view
    |> fun view -> Kernel.IO.StringView.shift view 6
    |> Result.unwrap
    |> Kernel.IO.StringView.sub ~off:0 ~len:3
    |> Result.unwrap
    |> Kernel.IO.StringView.to_string
  in
  if String.equal actual "rio" then
    Ok ()
  else
    Error "expected string view slicing to track offsets correctly"

let test_prefix_and_search = fun _ctx ->
  let view = Kernel.IO.StringView.from_string "GET /path HTTP/1.1\r\n\r\n" |> Result.unwrap in
  if
    Kernel.IO.StringView.starts_with view ~prefix:"GET "
    && Kernel.IO.StringView.index_char view ' ' = Some 3
    && Kernel.IO.StringView.index_string view "\r\n\r\n" = Some 18
  then
    Ok ()
  else
    Error "expected string view searches to find protocol delimiters"

let test_of_buffer_tracks_readable_region = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = Kernel.IO.StringView.from_buffer buffer |> Kernel.IO.StringView.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected string view of buffer to start at readable offset"

let tests = [
  Test.case "StringView roundtrips strings" test_of_string_roundtrip;
  Test.case "StringView sub and advance preserve offsets" test_sub_and_advance;
  Test.case "StringView prefix and search helpers find delimiters" test_prefix_and_search;
  Test.case "StringView of_buffer uses the readable region" test_of_buffer_tracks_readable_region;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_string_view_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
