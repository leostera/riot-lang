open Std
module Test = Std.Test
module Kernel = Kernel_new

let test_of_string_copies_input = fun _ctx ->
  let source = "riot" in
  let bytes = Kernel.Bytes.of_string source in
  Kernel.Bytes.set bytes 0 'R';
  if Kernel.String.equal source "riot" then
    Ok ()
  else
    Error "expected Bytes.of_string to keep the original string immutable"

let test_to_string_copies_bytes = fun _ctx ->
  let bytes = Kernel.Bytes.of_string "riot" in
  let snapshot = Kernel.Bytes.to_string bytes in
  Kernel.Bytes.set bytes 0 'R';
  if Kernel.String.equal snapshot "riot" then
    Ok ()
  else
    Error "expected Bytes.to_string to keep the returned string stable after byte mutation"

let test_sub_copies_selected_slice = fun _ctx ->
  let bytes = Kernel.Bytes.of_string "kernel" in
  let slice = Kernel.Bytes.sub bytes 1 4 in
  Kernel.Bytes.set bytes 2 'X';
  if Kernel.String.equal (Kernel.Bytes.to_string slice) "erne" then
    Ok ()
  else
    Error "expected Bytes.sub to keep its copied slice stable after source mutation"

let test_sub_string_returns_selected_slice = fun _ctx ->
  let bytes = Kernel.Bytes.of_string "kernel" in
  let slice = Kernel.Bytes.sub_string bytes 1 4 in
  if Kernel.String.equal slice "erne" then
    Ok ()
  else
    Error "expected Bytes.sub_string to return the selected immutable slice"

let tests = [
  Test.case "Bytes.of_string copies its input" test_of_string_copies_input;
  Test.case "Bytes.to_string copies its input" test_to_string_copies_bytes;
  Test.case "Bytes.sub copies the selected slice" test_sub_copies_selected_slice;
  Test.case "Bytes.sub_string returns the selected slice" test_sub_string_returns_selected_slice;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_bytes_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
