open Std

let test_diff_identical_lists () = Error "todo"
let test_diff_empty_lists () = Error "todo"
let test_diff_added_elements () = Error "todo"
let test_diff_removed_elements () = Error "todo"
let test_diff_changed_elements () = Error "todo"
let test_diff_reordered_elements () = Error "todo"
let test_diff_nested_lists () = Error "todo"
let test_diff_one_empty () = Error "todo"
let test_diff_different_lengths () = Error "todo"
let test_diff_mixed_changes () = Error "todo"

let tests = Test.[
  case "identical lists" test_diff_identical_lists;
  case "empty lists" test_diff_empty_lists;
  case "added elements" test_diff_added_elements;
  case "removed elements" test_diff_removed_elements;
  case "changed elements" test_diff_changed_elements;
  case "reordered elements" test_diff_reordered_elements;
  case "nested lists" test_diff_nested_lists;
  case "one empty" test_diff_one_empty;
  case "different lengths" test_diff_different_lengths;
  case "mixed changes" test_diff_mixed_changes;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"list-diff" ~tests ~args)
    ~args:Env.args
