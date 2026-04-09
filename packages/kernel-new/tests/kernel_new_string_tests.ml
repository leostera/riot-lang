open Std
module Test = Std.Test
module Kernel = Kernel_new

let test_of_bytes_copies_input = fun _ctx ->
  let bytes = Kernel.Bytes.of_string "riot" in
  let value = Kernel.String.of_bytes bytes in
  Kernel.Bytes.set bytes 0 'R';
  if Kernel.String.equal value "riot" then
    Ok ()
  else
    Error "expected String.of_bytes to keep the returned string stable after byte mutation"

let test_to_bytes_copies_input = fun _ctx ->
  let original = Kernel.String.append "ri" "ot" in
  let bytes = Kernel.String.to_bytes original in
  Kernel.Bytes.set bytes 0 'R';
  if Kernel.String.equal original "riot" then
    Ok ()
  else
    Error "expected String.to_bytes to keep the original string immutable"

let tests = [
  Test.case "String.of_bytes copies its input" test_of_bytes_copies_input;
  Test.case "String.to_bytes copies its input" test_to_bytes_copies_input;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_string_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
