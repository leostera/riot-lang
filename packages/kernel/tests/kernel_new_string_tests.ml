open Std

module Test = Std.Test
module Kernel = Kernel

let test_of_bytes_copies_input = fun _ctx ->
  let bytes = Kernel.Bytes.from_string "riot" in
  let value = Kernel.String.from_bytes bytes in
  let _ = Kernel.Bytes.set bytes ~at:0 ~char:'R' in
  if Kernel.String.equal value "riot" then
    Ok ()
  else
    Error "expected String.from_bytes to keep the returned string stable after byte mutation"

let test_to_bytes_copies_input = fun _ctx ->
  let original = Kernel.String.append "ri" "ot" in
  let bytes = Kernel.String.to_bytes original in
  let _ = Kernel.Bytes.set bytes ~at:0 ~char:'R' in
  if Kernel.String.equal original "riot" then
    Ok ()
  else
    Error "expected String.to_bytes to keep the original string immutable"

let test_init_builds_expected_string = fun _ctx ->
  let built =
    Kernel.String.init ~len:4 ~fn:(fun index -> Kernel.Char.from_int_unchecked (65 + index))
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

let test_capitalize_ascii_preserves_tail_casing = fun _ctx ->
  let lower_mixed = Kernel.String.capitalize_ascii "mutIterator" in
  let already_capitalized = Kernel.String.capitalize_ascii "MutIterator" in
  if
    Kernel.String.equal lower_mixed "MutIterator"
    && Kernel.String.equal already_capitalized "MutIterator"
  then
    Ok ()
  else
    Error "expected String.capitalize_ascii to uppercase only the first character and preserve the rest unchanged"

let test_equal_matches_only_identical_strings = fun _ctx ->
  if
    Kernel.String.equal "tests/fixtures" "tests/fixtures"
    && not (Kernel.String.equal "tests/fixtures" "tests/fixtures/")
    && not (Kernel.String.equal "tests/fixtures" "tests/generated")
  then
    Ok ()
  else
    Error "expected String.equal to match only byte-identical strings"

let test_starts_with_matches_exact_prefix_and_descendants = fun _ctx ->
  if
    Kernel.String.starts_with ~prefix:"tests/fixtures" "tests/fixtures"
    && Kernel.String.starts_with ~prefix:"tests/fixtures" "tests/fixtures/case.ml"
    && Kernel.String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures/case.ml"
    && not (Kernel.String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures")
  then
    Ok ()
  else
    Error "expected String.starts_with to match exact prefixes without inventing path-boundary rules"

let test_starts_with_is_raw_prefix_not_path_segment_match = fun _ctx ->
  if
    Kernel.String.starts_with ~prefix:"tests/fixtures" "tests/fixtures-generated"
    && Kernel.String.starts_with ~prefix:"tests/fixtures" "tests/fixtures_extra"
    && not (Kernel.String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures-generated")
  then
    Ok ()
  else
    Error "expected String.starts_with to use raw byte prefixes rather than path-aware matching"

let tests = [
  Test.case "String.from_bytes copies its input" test_of_bytes_copies_input;
  Test.case "String.to_bytes copies its input" test_to_bytes_copies_input;
  Test.case "String.init builds characters in order" test_init_builds_expected_string;
  Test.case "String.concat preserves separator order" test_concat_preserves_separator_order;
  Test.case
    "String.capitalize_ascii preserves tail casing"
    test_capitalize_ascii_preserves_tail_casing;
  Test.case "String.equal matches only identical strings" test_equal_matches_only_identical_strings;
  Test.case
    "String.starts_with matches exact prefixes and descendants"
    test_starts_with_matches_exact_prefix_and_descendants;
  Test.case
    "String.starts_with is raw prefix matching"
    test_starts_with_is_raw_prefix_not_path_segment_match;
]

let main ~args = Test.Cli.main ~name:"kernel_new_string_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
