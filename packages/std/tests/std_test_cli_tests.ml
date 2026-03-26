open Std

module Test = Std.Test

let sample_tests =
  [
    Test.case "alpha_long" (fun () -> Ok ());
    Test.case "beta" (fun () -> Ok ());
    Test.case "middle_long_case" (fun () -> Ok ());
  ]

let self_executable () =
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_test_cli_tests"

let split_lines output =
  output
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))

let parse_json_output stdout =
  Data.Json.of_string stdout |> Result.expect ~msg:"failed to parse json output"

let test_names_from_json stdout =
  let json = parse_json_output stdout in
  match Data.Json.get_field "tests" json with
  | Some (Data.Json.Array tests) ->
      tests
      |> List.filter_map (fun test_json ->
             match Data.Json.get_field "name" test_json with
             | Some (Data.Json.String name) -> Some name
             | _ -> None)
  | _ -> []

let run_sample_capture args =
  let cmd = Command.make (self_executable ()) ~args:("sample" :: args) in
  Command.output cmd |> Result.expect ~msg:"failed to run sample test cli"

let test_list_tests_lists_all_cases () =
  let output = run_sample_capture [ "list-tests" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-tests to succeed, got " ^ Int.to_string output.status)
  else
    let names = split_lines output.stdout |> List.sort String.compare in
    let expected =
      [ "alpha_long"; "beta"; "middle_long_case" ] |> List.sort String.compare
    in
    if names = expected then Ok ()
    else
      Error
        ("unexpected listed test names: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_suffix_substring () =
  let output =
    run_sample_capture [ "run-tests"; "--pattern"; "_long"; "--format"; "json" ]
  in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected =
      [ "alpha_long"; "middle_long_case" ] |> List.sort String.compare
    in
    if names = expected then Ok ()
    else
      Error
        ("unexpected filtered names for _long: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_middle_substring () =
  let output =
    run_sample_capture
      [ "run-tests"; "--pattern"; "long_case"; "--format"; "json" ]
  in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout in
    if names = [ "middle_long_case" ] then Ok ()
    else
      Error
        ("unexpected filtered names for long_case: "
       ^ String.concat ", " names)

let meta_tests =
  [
    Test.case "list-tests lists all sample cases" test_list_tests_lists_all_cases;
    Test.case
      "run-tests pattern matches suffix substring"
      test_run_tests_pattern_matches_suffix_substring;
    Test.case
      "run-tests pattern matches middle substring"
      test_run_tests_pattern_matches_middle_substring;
  ]

let sample_main ~args =
  match args with
  | exe :: _sample :: rest -> Test.Cli.main ~name:"sample" ~tests:sample_tests ~args:(exe :: rest)
  | _ -> Error (Failure "expected sample subcommand arguments")

let meta_main ~args =
  let normalize_args = function
    | [] -> [ "std_test_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_test_cli_tests" ~tests:meta_tests
    ~args:(normalize_args args)

let main ~args =
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ -> meta_main ~args

let () = Miniriot.run ~main ~args:Env.args ()
