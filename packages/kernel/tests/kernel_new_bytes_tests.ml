open Std

module Test = Std.Test
module Kernel = Kernel

let test_of_string_copies_input = fun _ctx ->
  let source = "riot" in
  let bytes = Kernel.Bytes.from_string source in
  let _ = Kernel.Bytes.set bytes ~at:0 ~char:'R' in
  if Kernel.String.equal source "riot" then
    Ok ()
  else
    Error "expected Bytes.from_string to keep the original string immutable"

let test_to_string_copies_bytes = fun _ctx ->
  let bytes = Kernel.Bytes.from_string "riot" in
  let snapshot = Kernel.Bytes.to_string bytes in
  let _ = Kernel.Bytes.set bytes ~at:0 ~char:'R' in
  if Kernel.String.equal snapshot "riot" then
    Ok ()
  else
    Error "expected Bytes.to_string to keep the returned string stable after byte mutation"

let test_sub_copies_selected_slice = fun _ctx ->
  let bytes = Kernel.Bytes.from_string "kernel" in
  match Kernel.Bytes.sub bytes ~offset:1 ~len:4 with
  | Ok slice ->
      let _ = Kernel.Bytes.set bytes ~at:2 ~char:'X' in
      if Kernel.String.equal (Kernel.Bytes.to_string slice) "erne" then
        Ok ()
      else
        Error "expected Bytes.sub to keep its copied slice stable after source mutation"
  | Error _ -> Error "expected Bytes.sub to copy a valid slice"

let test_sub_string_returns_selected_slice = fun _ctx ->
  let bytes = Kernel.Bytes.from_string "kernel" in
  let slice = Kernel.Bytes.sub_string bytes ~offset:1 ~len:4 in
  if Kernel.String.equal slice "erne" then
    Ok ()
  else
    Error "expected Bytes.sub_string to return the selected immutable slice"

let tests = [
  Test.case "Bytes.from_string copies its input" test_of_string_copies_input;
  Test.case "Bytes.to_string copies its input" test_to_string_copies_bytes;
  Test.case "Bytes.sub copies the selected slice" test_sub_copies_selected_slice;
  Test.case "Bytes.sub_string returns the selected slice" test_sub_string_returns_selected_slice;
]

let main ~args = Test.Cli.main ~name:"kernel_new_bytes_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
