open Std

module Test = Std.Test

let sample_benchmarks =
  let config: Bench.bench_config = { iterations = 1; warmup = 0 } in
  [
    Bench.with_config ~config "alpha_long" (fun () -> ());
    Bench.with_config ~config "beta" (fun () -> ());
    Bench.with_config ~config "middle_long_case" (fun () -> ());
  ]

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_bench_cli_tests"

let run_sample_capture = fun args ->
  let cmd = Command.make (self_executable ()) ~args:("sample" :: args) in
  Command.output cmd
  |> Result.expect ~msg:"failed to run sample bench cli"

let json_output_lines = fun stdout ->
  stdout
  |> String.split ~by:"\n"
  |> List.filter ~fn:(fun line -> not (String.equal (String.trim line) ""))

let parse_json_output = fun stdout ->
  let line =
    json_output_lines stdout
    |> List.rev
    |> List.get ~at:0
    |> Option.unwrap_or ~default:stdout
  in
  Data.Json.from_string line
  |> Result.expect ~msg:"failed to parse json output"

let assoc_value = fun key entries ->
  match List.find entries ~fn:(fun (entry_key, _) -> String.equal entry_key key) with
  | Some (_, value) -> Some value
  | None -> None

let bench_names_from_json = fun stdout ->
  let json = parse_json_output stdout in
  match Data.Json.get_field "benchmarks" json with
  | Some (Data.Json.Array benchmarks) ->
      benchmarks
      |> List.filter_map
        ~fn:(fun benchmark_json ->
          match Data.Json.get_field "name" benchmark_json with
          | Some (Data.Json.String name) -> Some name
          | _ -> None)
  | _ -> []

let listed_benchmark_fields_from_json = fun stdout ->
  let json = parse_json_output stdout in
  match Data.Json.get_field "benchmarks" json with
  | Some (Data.Json.Array benchmarks) ->
      benchmarks
      |> List.filter_map
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Data.Json.Object fields -> Some fields
          | _ -> None)
  | _ -> []

let test_list_benchmarks_lists_all_cases = fun _ctx ->
  let output = run_sample_capture [ "list-benchmarks" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-benchmarks to succeed, got " ^ Int.to_string output.status)
  else
    let lines =
      output.stdout
      |> String.split ~by:"\n"
      |> List.filter ~fn:(fun line -> not (String.equal line ""))
      |> List.sort ~compare:String.compare
    in
    let expected =
      [ "alpha_long"; "beta"; "middle_long_case" ]
      |> List.sort ~compare:String.compare
    in
    if lines = expected then
      Ok ()
    else
      Error ("unexpected listed benchmark names: " ^ String.concat ", " lines)

let test_list_benchmarks_json_includes_metadata = fun _ctx ->
  let output = run_sample_capture [ "list-benchmarks"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-benchmarks --json to succeed, got " ^ Int.to_string output.status)
  else
    match listed_benchmark_fields_from_json output.stdout with
    | first :: _ ->
        let has name value = assoc_value name first = Some value in
        if
          has "index" (Data.Json.Int 1)
          && has "name" (Data.Json.String "alpha_long")
          && has "kind" (Data.Json.String "benchmark")
          && has "iterations" (Data.Json.Int 1)
          && has "warmup" (Data.Json.Int 0)
          && has "skip" (Data.Json.Bool false)
        then
          Ok ()
        else
          Error "expected list-benchmarks --json to include metadata fields"
    | [] -> Error "expected list-benchmarks --json to include benchmarks"

let test_list_benchmarks_respects_pattern = fun _ctx ->
  let output = run_sample_capture [ "list-benchmarks"; "--json"; "_long" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered list-benchmarks --json to succeed, got " ^ Int.to_string output.status)
  else
    let names =
      bench_names_from_json output.stdout
      |> List.sort ~compare:String.compare
    in
    let expected =
      [ "alpha_long"; "middle_long_case" ]
      |> List.sort ~compare:String.compare
    in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered benchmark names: " ^ String.concat ", " names)

let test_run_benchmarks_pattern_matches_substring = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark run to succeed, got " ^ Int.to_string output.status)
  else if
    String.contains output.stdout "Running 2 benchmarks"
    && String.contains output.stdout "[1] alpha_long:"
    && String.contains output.stdout "[2] middle_long_case:"
    && not (String.contains output.stdout "beta:")
  then
    Ok ()
  else
    Error "unexpected filtered benchmark output for _long"

let test_run_benchmarks_succeeds_with_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "missing_case" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark run with no matches to succeed, got "
    ^ Int.to_string output.status)
  else if
    String.contains output.stdout "Running 0 benchmarks"
    && String.contains output.stdout "Summary: 0 total"
  then
    Ok ()
  else
    Error "expected no-match benchmark run to report zero benchmarks"

let test_run_benchmarks_json_flag_filters_results = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark json run to succeed, got " ^ Int.to_string output.status)
  else
    let names =
      bench_names_from_json output.stdout
      |> List.sort ~compare:String.compare
    in
    let expected =
      [ "alpha_long"; "middle_long_case" ]
      |> List.sort ~compare:String.compare
    in
    if names = expected then
      Ok ()
    else
      Error ("unexpected filtered benchmark names for _long: " ^ String.concat ", " names)

let test_run_benchmarks_json_flag_reports_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "missing_case"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected zero-match benchmark json run to succeed, got " ^ Int.to_string output.status)
  else if bench_names_from_json output.stdout = [] then
    Ok ()
  else
    Error "expected zero-match benchmark json run to report an empty benchmark list"

let test_run_benchmarks_json_includes_timing_fields = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark json run to succeed, got " ^ Int.to_string output.status)
  else
    let json = parse_json_output output.stdout in
    let has_int_field name json =
      match Data.Json.get_field name json with
      | Some (Data.Json.Int _) -> true
      | _ -> false
    in
    if
      has_int_field "started_at_us" json
      && has_int_field "completed_at_us" json
      && has_int_field "duration_us" json
    then
      Ok ()
    else
      Error "expected benchmark json output to include timing fields"

let test_run_benchmarks_json_includes_gc_fields = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark json run to succeed, got " ^ Int.to_string output.status)
  else
    match Data.Json.get_field "benchmarks" (parse_json_output output.stdout) with
    | Some (Data.Json.Array ((Data.Json.Object first) :: _)) ->
        (match Data.Json.get_field "statistics" (Data.Json.Object first) with
        | Some (Data.Json.Object stats) ->
            (match assoc_value "gc" stats with
            | Some (Data.Json.Object gc_fields) ->
                if (
                  assoc_value "minor_collections" gc_fields
                  |> Option.is_some
                )
                && (
                  assoc_value "major_collections" gc_fields
                  |> Option.is_some
                )
                && (
                  assoc_value "compactions" gc_fields
                  |> Option.is_some
                ) then
                  Ok ()
                else
                  Error "expected benchmark json output to include gc fields"
            | _ -> Error "expected benchmark statistics to include a gc object")
        | _ -> Error "expected completed benchmark statistics in benchmark json output"
        )
    | _ -> Error "expected benchmark json output to include at least one benchmark"

let test_run_benchmarks_json_emits_case_started_progress = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long"; "--json" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark json run to succeed, got " ^ Int.to_string output.status)
  else
    let progress_lines =
      json_output_lines output.stdout
      |> List.filter_map
        ~fn:(fun line ->
          match Data.Json.from_string line with
          | Ok (Data.Json.Object _ as json) -> Some json
          | Ok _
          | Error _ -> None)
      |> List.filter
        ~fn:(fun json ->
          match Data.Json.get_field "type" json with
          | Some (Data.Json.String "BenchCaseStarted") -> true
          | _ -> false)
    in
    match progress_lines with
    | first :: _ ->
        if
          Data.Json.get_field "index" first = Some (Data.Json.Int 1)
          && Data.Json.get_field "name" first = Some (Data.Json.String "alpha_long")
          && Data.Json.get_field "iterations" first = Some (Data.Json.Int 1)
          && Data.Json.get_field "warmup" first = Some (Data.Json.Int 0)
        then
          Ok ()
        else
          Error "expected BenchCaseStarted progress to include benchmark metadata"
    | [] -> Error "expected run-benchmarks --json to emit BenchCaseStarted progress"

let meta_tests = [
  Test.case "list-benchmarks lists all sample cases" test_list_benchmarks_lists_all_cases;
  Test.case "list-benchmarks --json includes metadata" test_list_benchmarks_json_includes_metadata;
  Test.case "list-benchmarks respects pattern" test_list_benchmarks_respects_pattern;
  Test.case "run-benchmarks pattern matches substring" test_run_benchmarks_pattern_matches_substring;
  Test.case
    "run-benchmarks succeeds when the query matches no benchmarks"
    test_run_benchmarks_succeeds_with_zero_matches;
  Test.case "run-benchmarks --json filters results" test_run_benchmarks_json_flag_filters_results;
  Test.case
    "run-benchmarks --json reports zero matches"
    test_run_benchmarks_json_flag_reports_zero_matches;
  Test.case
    "run-benchmarks --json includes timing fields"
    test_run_benchmarks_json_includes_timing_fields;
  Test.case "run-benchmarks --json includes gc fields" test_run_benchmarks_json_includes_gc_fields;
  Test.case
    "run-benchmarks --json emits case started progress"
    test_run_benchmarks_json_emits_case_started_progress;
]

let sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest ->
      Bench.Cli.main ~name:"sample" ~benchmarks:sample_benchmarks ~args:(exe :: rest)
  | _ -> Error (Failure "expected sample subcommand arguments")

let meta_main = fun ~args ->
  let normalize_args = fun __tmp1 ->
    match __tmp1 with
    | [] -> [ "std_bench_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_bench_cli_tests" ~tests:meta_tests ~args:(normalize_args args) ()

let main ~args =
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ -> meta_main ~args

let () = Runtime.run ~main ~args:Env.args ()
