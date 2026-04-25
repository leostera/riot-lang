open Std

module Test = Std.Test

let test_equal_matches_only_identical_strings = fun _ctx ->
  if String.equal "tests/fixtures" "tests/fixtures" && not (String.equal "tests/fixtures" "tests/fixtures/") && not (String.equal "tests/fixtures" "tests/generated") then
    Ok ()
  else Error "expected Std.String.equal to match only byte-identical strings"

let test_starts_with_matches_exact_prefix_and_descendants = fun _ctx ->
  if String.starts_with ~prefix:"tests/fixtures" "tests/fixtures" && String.starts_with ~prefix:"tests/fixtures" "tests/fixtures/case.ml" && String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures/case.ml" && not (String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures") then
    Ok ()
  else Error "expected Std.String.starts_with to match exact prefixes without path-aware semantics"

let test_starts_with_is_raw_prefix_not_path_segment_match = fun _ctx ->
  if String.starts_with ~prefix:"tests/fixtures" "tests/fixtures-generated" && String.starts_with ~prefix:"tests/fixtures" "tests/fixtures_extra" && not (String.starts_with ~prefix:"tests/fixtures/" "tests/fixtures-generated") then
    Ok ()
  else Error "expected Std.String.starts_with to use raw byte prefixes rather than path-segment matching"

let tests = [ Test.case "Std.String.equal matches only identical strings" test_equal_matches_only_identical_strings; Test.case "Std.String.starts_with matches exact prefixes and descendants" test_starts_with_matches_exact_prefix_and_descendants; Test.case "Std.String.starts_with is raw prefix matching" test_starts_with_is_raw_prefix_not_path_segment_match ]

let main ~args = Test.Cli.main ~name:"std_string_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
