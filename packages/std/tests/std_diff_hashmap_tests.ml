open Std

let test_diff_identical_hashmaps () = Error "todo"
let test_diff_empty_hashmaps () = Error "todo"
let test_diff_added_keys () = Error "todo"
let test_diff_removed_keys () = Error "todo"
let test_diff_changed_values () = Error "todo"
let test_diff_mixed_changes () = Error "todo"
let test_diff_nested_hashmaps () = Error "todo"
let test_diff_one_empty () = Error "todo"
let test_diff_different_sizes () = Error "todo"
let test_diff_all_different () = Error "todo"

let tests = Test.[
  case "identical hashmaps" test_diff_identical_hashmaps;
  case "empty hashmaps" test_diff_empty_hashmaps;
  case "added keys" test_diff_added_keys;
  case "removed keys" test_diff_removed_keys;
  case "changed values" test_diff_changed_values;
  case "mixed changes" test_diff_mixed_changes;
  case "nested hashmaps" test_diff_nested_hashmaps;
  case "one empty" test_diff_one_empty;
  case "different sizes" test_diff_different_sizes;
  case "all different" test_diff_all_different;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"hashmap-diff" ~tests ~args)
    ~args:Env.args
