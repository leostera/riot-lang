open Std

let test_diff_identical_strings () = Error "todo"
let test_diff_different_strings () = Error "todo"
let test_diff_empty_strings () = Error "todo"
let test_diff_one_empty () = Error "todo"
let test_diff_char_by_char () = Error "todo"
let test_diff_inserted_chars () = Error "todo"
let test_diff_deleted_chars () = Error "todo"
let test_diff_replaced_chars () = Error "todo"
let test_diff_case_change () = Error "todo"
let test_diff_whitespace_changes () = Error "todo"

let tests = Test.[
  case "identical strings" test_diff_identical_strings;
  case "different strings" test_diff_different_strings;
  case "empty strings" test_diff_empty_strings;
  case "one empty" test_diff_one_empty;
  case "char by char" test_diff_char_by_char;
  case "inserted chars" test_diff_inserted_chars;
  case "deleted chars" test_diff_deleted_chars;
  case "replaced chars" test_diff_replaced_chars;
  case "case change" test_diff_case_change;
  case "whitespace changes" test_diff_whitespace_changes;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"string-diff" ~tests ~args)
    ~args:Env.args
