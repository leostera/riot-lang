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

let test_init_builds_expected_string = fun _ctx ->
  let built =
    Kernel.String.init 4 (fun index -> Kernel.Char.unsafe_of_int (65 + index))
  in
  if Kernel.String.equal built "ABCD" then
    Ok ()
  else
    Error "expected String.init to build characters in index order"

let test_concat_preserves_separator_order = fun _ctx ->
  let value = Kernel.String.concat "/" [ "domains"; "admin"; "users" ] in
  if Kernel.String.equal value "domains/admin/users" then
    Ok ()
  else
    Error "expected String.concat to preserve value and separator order"

let tests = [
  Test.case "String.of_bytes copies its input" test_of_bytes_copies_input;
  Test.case "String.to_bytes copies its input" test_to_bytes_copies_input;
  Test.case "String.init builds characters in order" test_init_builds_expected_string;
  Test.case "String.concat preserves separator order" test_concat_preserves_separator_order;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_string_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
