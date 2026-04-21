open Std
module Kernel = Kernel
module Test = Std.Test

let test_of_string_roundtrip = fun _ctx ->
  let slice = Kernel.IO.IoVec.IoSlice.from_string "hello riot" |> Result.unwrap in
  if
    Kernel.IO.IoVec.IoSlice.length slice = 10
    && Kernel.IO.IoVec.IoSlice.get slice ~at:4
    = Ok 'o' && String.equal (Kernel.IO.IoVec.IoSlice.to_string slice) "hello riot"
  then
    Ok ()
  else
    Error "expected IoSlice to preserve string contents"

let test_sub_and_advance = fun _ctx ->
  let slice = Kernel.IO.IoVec.IoSlice.from_string "hello riot" |> Result.unwrap in
  let actual = slice
  |> fun slice ->
    Kernel.IO.IoVec.IoSlice.shift slice 6
    |> Result.unwrap
    |> Kernel.IO.IoVec.IoSlice.sub ~off:0 ~len:3
    |> Result.unwrap
    |> Kernel.IO.IoVec.IoSlice.to_string in
  if String.equal actual "rio" then
    Ok ()
  else
    Error "expected IoSlice slicing to track offsets correctly"

let test_prefix_and_search = fun _ctx ->
  let slice = Kernel.IO.IoVec.IoSlice.from_string "GET /path HTTP/1.1\r\n\r\n" |> Result.unwrap in
  if
    Kernel.IO.IoVec.IoSlice.starts_with slice ~prefix:"GET "
    && Kernel.IO.IoVec.IoSlice.index_char slice ' ' = Some 3
    && Kernel.IO.IoVec.IoSlice.index_string slice "\r\n\r\n" = Some 18
  then
    Ok ()
  else
    Error "expected IoSlice searches to find protocol delimiters"

let test_of_buffer_tracks_readable_region = fun _ctx ->
  let buffer = Kernel.IO.Buffer.create () |> Result.unwrap in
  let _ = Kernel.IO.Buffer.append_string buffer "hello riot" |> Result.unwrap in
  let _ = Kernel.IO.Buffer.consume buffer ~len:6 |> Result.unwrap in
  let actual = Kernel.IO.Buffer.readable buffer |> Kernel.IO.IoVec.IoSlice.to_string in
  if String.equal actual "riot" then
    Ok ()
  else
    Error "expected readable IoSlice of buffer to start at readable offset"

let tests = [
  Test.case "IoSlice roundtrips strings" test_of_string_roundtrip;
  Test.case "IoSlice sub and advance preserve offsets" test_sub_and_advance;
  Test.case "IoSlice prefix and search helpers find delimiters" test_prefix_and_search;
  Test.case "readable IoSlice uses the readable region" test_of_buffer_tracks_readable_region;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_io_slice_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
