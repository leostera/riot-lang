open Std
module Test = Std.Test

let flaky_counter = Sync.Atomic.make 0

let sample_tests = [
  Test.case ~size:Test.Long "alpha_long" (fun _ctx -> Ok ());
  Test.case "beta" (fun _ctx -> Ok ());
  Test.case ~size:Test.Long "middle_long_case" (fun _ctx -> Ok ());
  Test.case
    ~reliability:Test.(Flaky { retry_attempts = 2 })
    "flaky_then_ok"
    (fun _ctx ->
      if Sync.Atomic.fetch_and_add flaky_counter 1 = 0 then
        Error "transient failure"
      else
        Ok ());
  Test.case "timeout_probe" (fun _ctx ->
    sleep (Time.Duration.from_millis 20);
    Ok ());
]

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_test_cli_tests"

let split_lines = fun output ->
  output |> String.split_on_char '\n' |> List.filter (fun line -> not (String.equal line ""))

let parse_json_output = fun stdout -> Data.Json.of_string stdout |> Result.expect ~msg:"failed to parse json output"

let test_names_from_json = fun stdout ->
  let json = parse_json_output stdout in
  match Data.Json.get_field "tests" json with
  | Some (Data.Json.Array tests) ->
      tests |> List.filter_map
        (fun test_json ->
          match Data.Json.get_field "name" test_json with
          | Some (Data.Json.String name) -> Some name
          | _ -> None)
  | _ -> []

let run_sample_capture = fun args ->
  let cmd = Command.make (self_executable ()) ~args:(("sample" :: args)) in
  Command.output cmd |> Result.expect ~msg:"failed to run sample test cli"

let test_list_tests_lists_all_cases = fun _ctx ->
  let output = run_sample_capture [ "list-tests" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-tests to succeed, got " ^ Int.to_string output.status)
  else
    let names = split_lines output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "beta"; "middle_long_case"; "flaky_then_ok"; "timeout_probe" ]
    |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected listed test names: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_suffix_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_long"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "middle_long_case" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for _long: " ^ String.concat ", " names)

let test_run_tests_pattern_matches_middle_substring = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "long_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout in
    if names = [ "middle_long_case" ] then
      Ok ()
    else
      Error ("unexpected filtered names for long_case: " ^ String.concat ", " names)

let test_run_tests_returns_success_with_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "missing_case"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered run with no matches to succeed, got " ^ Int.to_string output.status)
  else if test_names_from_json output.stdout = [] then
    Ok ()
  else
    Error "expected filtered run with no matches to report an empty test list"

let test_run_tests_json_flag_alias_emits_json = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --json run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "middle_long_case" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --json: " ^ String.concat ", " names)

let test_run_tests_small_flag_filters_small_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--small"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --small run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected = [ "beta"; "flaky_then_ok"; "timeout_probe" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --small: " ^ String.concat ", " names)

let test_run_tests_long_flag_filters_long_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--long"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --long run to succeed, got " ^ Int.to_string output.status)
  else
    let names = test_names_from_json output.stdout |> List.sort String.compare in
    let expected = [ "alpha_long"; "middle_long_case" ] |> List.sort String.compare in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered names for --long: " ^ String.concat ", " names)

let test_run_tests_flaky_flag_filters_flaky_tests = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "--flaky"; "--format"; "json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --flaky run to succeed, got " ^ Int.to_string output.status)
  else if test_names_from_json output.stdout = [ "flaky_then_ok" ] then
    Ok ()
  else
    Error ("unexpected filtered names for --flaky: " ^ String.concat ", " (test_names_from_json output.stdout))

let test_run_tests_json_includes_timing_fields = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected --json run to succeed, got " ^ Int.to_string output.status)
  else
    let json = parse_json_output output.stdout in
    let has_int_field name json =
      match Data.Json.get_field name json with
      | Some (Data.Json.Int _) -> true
      | _ -> false
    in
    let tests_have_duration =
      match Data.Json.get_field "tests" json with
      | Some (Data.Json.Array tests) -> List.for_all
        (fun test_json -> has_int_field "duration_us" test_json)
        tests
      | _ -> false
    in
    let summary_has_duration =
      match Data.Json.get_field "summary" json with
      | Some summary_json -> has_int_field "duration_us" summary_json
      | None -> false
    in
    if
      has_int_field "started_at_us" json
      && has_int_field "completed_at_us" json
      && has_int_field "duration_us" json
      && summary_has_duration
      && tests_have_duration
    then
      Ok ()
    else
      Error "expected test json output to include timing fields"

let test_run_tests_json_includes_reliability_metadata = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "flaky_then_ok"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected flaky --json run to succeed, got " ^ Int.to_string output.status)
  else
    let json = parse_json_output output.stdout in
    match Data.Json.get_field "tests" json with
    | Some (Data.Json.Array [ Data.Json.Object fields ]) ->
        let has name value = List.assoc_opt name fields = Some value in
        if
          has "size" (Data.Json.String "small")
          && has "reliability" (Data.Json.String "flaky")
          && has "retry_attempts" (Data.Json.Int 2)
          && has "attempts" (Data.Json.Int 2)
          && has "status" (Data.Json.String "passed")
        then
          Ok ()
        else
          Error "expected flaky json output to include reliability metadata"
    | _ ->
        Error "expected exactly one flaky test in json output"

let test_run_tests_small_timeout_reports_timed_out = fun _ctx ->
  let output = run_sample_capture [ "run-tests"; "timeout_probe"; "--json"; "--small-timeout-ms"; "1" ] in
  if Int.equal output.status 0 then
    Error "expected timeout probe to fail"
  else
    let json = parse_json_output output.stdout in
    match Data.Json.get_field "tests" json with
    | Some (Data.Json.Array [ Data.Json.Object fields ]) ->
        let has name value = List.assoc_opt name fields = Some value in
        if
          has "status" (Data.Json.String "timed_out")
          && has "timeout_ms" (Data.Json.Int 1)
        then
          Ok ()
        else
          Error "expected timeout probe json output to report a timeout"
    | _ ->
        Error "expected exactly one timeout probe test in json output"

let meta_tests = [
  Test.case "list-tests lists all sample cases" test_list_tests_lists_all_cases;
  Test.case "run-tests pattern matches suffix substring" test_run_tests_pattern_matches_suffix_substring;
  Test.case "run-tests pattern matches middle substring" test_run_tests_pattern_matches_middle_substring;
  Test.case "run-tests succeeds when the query matches no tests" test_run_tests_returns_success_with_zero_matches;
  Test.case "run-tests --json alias emits json" test_run_tests_json_flag_alias_emits_json;
  Test.case "run-tests --small filters small tests" test_run_tests_small_flag_filters_small_tests;
  Test.case "run-tests --long filters long tests" test_run_tests_long_flag_filters_long_tests;
  Test.case "run-tests --flaky filters flaky tests" test_run_tests_flaky_flag_filters_flaky_tests;
  Test.case "run-tests --json includes timing fields" test_run_tests_json_includes_timing_fields;
  Test.case "run-tests --json includes reliability metadata" test_run_tests_json_includes_reliability_metadata;
  Test.case "run-tests --small-timeout-ms reports timed out tests" test_run_tests_small_timeout_reports_timed_out;
]

let sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest -> Test.Cli.main ~name:"sample" ~tests:sample_tests ~args:((exe :: rest))
  | _ -> Error (Failure "expected sample subcommand arguments")

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_test_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_test_cli_tests" ~tests:meta_tests ~args:(normalize_args args)

let main = fun ~args ->
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ -> meta_main ~args

let () = Actors.run ~main ~args:Env.args ()
