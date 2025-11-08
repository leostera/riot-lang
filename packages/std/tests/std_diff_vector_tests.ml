open Std
open Std.Data
open Std.Collections

let test_diff_identical_vectors () = Error "todo"
let test_diff_empty_vectors () = Error "todo"
let test_diff_added_elements () = Error "todo"
let test_diff_removed_elements () = Error "todo"
let test_diff_changed_elements () = Error "todo"
let test_diff_different_lengths () = Error "todo"
let test_diff_one_empty () = Error "todo"
let test_diff_all_different () = Error "todo"
let test_diff_nested_vectors () = Error "todo"
let test_diff_mixed_changes () = Error "todo"

let tests =
  Test.
    [
      case "identical vectors" test_diff_identical_vectors;
      case "empty vectors" test_diff_empty_vectors;
      case "added elements" test_diff_added_elements;
      case "removed elements" test_diff_removed_elements;
      case "changed elements" test_diff_changed_elements;
      case "different lengths" test_diff_different_lengths;
      case "one empty" test_diff_one_empty;
      case "all different" test_diff_all_different;
      case "nested vectors" test_diff_nested_vectors;
      case "mixed changes" test_diff_mixed_changes;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"vector-diff" ~tests ~args)
    ~args:Env.args ()
