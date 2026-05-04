open Std

module Test = Std.Test
module Kernel = Kernel

let test_of_string_roundtrips_raw_text = fun _ctx ->
  let raw = "domains/admin/users/models/testing/user.ml" in
  if Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.from_string raw)) raw then
    Ok ()
  else
    Error "expected Path.from_string and Path.to_string to roundtrip raw path text"

let test_join_treats_empty_sides_as_identity = fun _ctx ->
  let raw = "domains/admin" in
  if
    Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.join "" raw)) raw
    && Kernel.String.equal (Kernel.Path.to_string (Kernel.Path.join raw "")) raw
  then
    Ok ()
  else
    Error "expected Path.join to treat empty sides as identity"

let test_join_avoids_duplicate_separators = fun _ctx ->
  let value = Kernel.Path.to_string (Kernel.Path.join "domains/" "admin") in
  if Kernel.String.equal value "domains/admin" then
    Ok ()
  else
    Error "expected Path.join to avoid inserting an extra separator"

let test_infix_join_matches_function = fun _ctx ->
  let left = Kernel.Path.from_string "domains" in
  let right = Kernel.Path.from_string "admin" in
  if
    Kernel.String.equal
      (Kernel.Path.to_string Kernel.Path.(left / right))
      (Kernel.Path.to_string (Kernel.Path.join left right))
  then
    Ok ()
  else
    Error "expected Path./ to match Path.join"

let tests = [
  Test.case "Path.from_string roundtrips raw text" test_of_string_roundtrips_raw_text;
  Test.case "Path.join treats empty sides as identity" test_join_treats_empty_sides_as_identity;
  Test.case "Path.join avoids duplicate separators" test_join_avoids_duplicate_separators;
  Test.case "Path./ matches Path.join" test_infix_join_matches_function;
]

let main ~args = Test.Cli.main ~name:"kernel_new_path_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
