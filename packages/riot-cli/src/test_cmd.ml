open Std
open Riot_model
open Riot_build
open ArgParser

let command =
  let open ArgParser in
    let open Arg in
      command "test"
      |> about "Run tests with optional substring matching"
      |> ArgParser.allow_trailing_args
      |> args
        [ positional "pattern" |> required false |> help
            "Optional test query. Use package:suite to run one suite, or \
               -p/--package to limit execution to one package. Omit to run all tests."; option "package"
          |> short 'p'
          |> long "package"
          |> help "Run tests from a specific package"; flag "release" |> long "release" |> help "Use the release build profile"; flag "small" |> long "small" |> help "Run only tests marked small"; flag
            "large"
          |> long "large"
          |> help "Run only tests marked large"; flag "flaky" |> long "flaky" |> help "Run only tests marked flaky"; flag
            "json"
          |> long "json"
          |> help "Emit machine-readable JSONL events"; flag "verbose"
          |> short 'v'
          |> long "verbose"
          |> help "Enable verbose output for tests"
          |> count; ]

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_empty_hint = fun package_filter suite_filter ->
  match (package_filter, suite_filter) with
  | (Some package_name, Some suite_name) -> println
    ("No test suite '" ^ suite_name ^ "' found in package '" ^ package_name ^ "'")
  | (Some package_name, None) -> println ("No test suites found in package '" ^ package_name ^ "'")
  | (None, Some suite_name) -> println ("No test suites named '" ^ suite_name ^ "' found")
  | (None, None) -> println "No test binaries found"

type slow_test = {
  suite_label: string;
  test_name: string;
  duration_us: int;
}

type failed_test = {
  suite_label: string;
  test_name: string;
  message: string;
  duration_us: int;
}

type timing_summary = {
  mutable measured_duration_us: int;
  mutable measured_test_count: int;
  mutable slowest_tests: slow_test list;
  mutable failed_tests: failed_test list;
}

let empty_timing_summary = fun () ->
  { measured_duration_us = 0; measured_test_count = 0; slowest_tests = []; failed_tests = [] }

let take = fun limit values ->
  let rec loop remaining acc rest =
    match (remaining, rest) with
    | (0, _) -> List.rev acc
    | (_, []) -> List.rev acc
    | (_, value :: tail) -> loop (remaining - 1) (value :: acc) tail
  in
  loop limit [] values

let format_duration_us = fun duration_us ->
  if duration_us < 1_000 then
    Int.to_string duration_us ^ "us"
  else if duration_us < 1_000_000 then
    Float.to_string ~precision:2 (float_of_int duration_us /. 1000.0) ^ "ms"
  else
    Float.to_string ~precision:2 (float_of_int duration_us /. 1000000.0) ^ "s"

let metadata_labels = fun size reliability ->
  let size_labels =
    match size with
    | Riot_build.Small -> []
    | Riot_build.Large -> [ "large" ]
  in
  let reliability_labels =
    match reliability with
    | Riot_build.Stable -> []
    | Riot_build.Flaky { retry_attempts } -> [ "flaky/" ^ Int.to_string retry_attempts ]
  in
  List.append size_labels reliability_labels

let metadata_suffix = fun size reliability ->
  match metadata_labels size reliability with
  | [] -> ""
  | labels -> " [" ^ String.concat " " labels ^ "]"

let attempts_suffix = fun attempts ->
  if attempts <= 1 then
    ""
  else
    " after " ^ Int.to_string attempts ^ " attempts"

let timeout_message = fun timeout_ms -> "timed out after " ^ Int.to_string timeout_ms ^ "ms"

let record_suite_timing = fun (timing: timing_summary) ~suite_label (
  summary: Riot_build.test_suite_summary
) ->
  timing.measured_duration_us <- timing.measured_duration_us + summary.duration_us;
  timing.measured_test_count <- timing.measured_test_count + summary.total;
  let slow_suite_tests: slow_test list = summary.results
  |> List.map
    (fun (result: Riot_build.test_case_result) ->
      ({ suite_label; test_name = result.name; duration_us = result.duration_us }: slow_test)) in
  let slowest_tests: slow_test list =
    List.append timing.slowest_tests slow_suite_tests
    |> List.sort
      (fun (left: slow_test) (right: slow_test) ->
        Int.compare right.duration_us left.duration_us)
    |> take 5
  in
  timing.slowest_tests <- slowest_tests;
  timing.failed_tests <- List.rev_append
    (
      summary.results |> List.filter_map
        (fun (result: Riot_build.test_case_result) ->
          match result.result with
          | Riot_build.Failed message -> Some (
            { suite_label; test_name = result.name; message; duration_us = result.duration_us }: failed_test
          )
          | Riot_build.Timed_out { timeout_ms } -> Some (
            {
              suite_label;
              test_name = result.name;
              message = timeout_message timeout_ms;
              duration_us = result.duration_us
            }:
              failed_test
          )
          | Riot_build.Passed
          | Riot_build.Skipped -> None)
    )
    timing.failed_tests

let print_summary = fun ~label ~total ~passed ~failed ~skipped ~(timing:timing_summary) ->
  println "";
  println label;
  println ("  Total test cases: " ^ Int.to_string total);
  println ("  Passed: " ^ Int.to_string passed);
  println ("  Failed: " ^ Int.to_string failed);
  println ("  Skipped: " ^ Int.to_string skipped);
  if timing.measured_test_count > 0 then
    (
      println ("  Measured test time: " ^ format_duration_us timing.measured_duration_us);
      println
        ("  Average per test: "
        ^ format_duration_us (timing.measured_duration_us / timing.measured_test_count));
      println "  Slowest tests:";
      timing.slowest_tests
      |> List.iteri
        (fun idx (test: slow_test) ->
          println
            ("    "
            ^ Int.to_string (idx + 1)
            ^ ". "
            ^ test.suite_label
            ^ " :: "
            ^ test.test_name
            ^ " ("
            ^ format_duration_us test.duration_us
            ^ ")"))
    );
  if not (List.is_empty timing.failed_tests) then
    (
      println "  Failed tests:";
      timing.failed_tests |> List.rev |> List.iteri
        (fun idx (test: failed_test) ->
          println
            ("    "
            ^ Int.to_string (idx + 1)
            ^ ". "
            ^ test.suite_label
            ^ " :: "
            ^ test.test_name
            ^ " ("
            ^ format_duration_us test.duration_us
            ^ ")");
          if not (String.equal test.message "") then
            println ("       " ^ test.message))
    )

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at |> Time.Duration.to_micros

let json_int_field = fun name fields ->
  match List.assoc_opt name fields with
  | Some (Data.Json.Int value) -> Some value
  | _ -> None

let upsert_int_field = fun name value fields ->
  let filtered =
    List.filter (fun (field_name, _) -> not (String.equal field_name name)) fields
  in
  filtered @ [ (name, Data.Json.Int value) ]

let stamp_json_event = fun ~command_started_at ~duration_us (event: Riot_build.test_event) (
  json: Data.Json.t
) ->
  match json with
  | Data.Json.Object fields ->
      let elapsed_us = event_elapsed_us ~command_started_at in
      let duration_us =
        match duration_us with
        | Some duration_us -> duration_us
        | None -> Option.unwrap_or ~default:0 (json_int_field "duration_us" fields)
      in
      let fields = upsert_int_field "duration_us" duration_us fields in
      let fields =
        match event with
        | Riot_build.RunningSuite _ -> upsert_int_field "started_at_us" elapsed_us fields
        | Riot_build.SuiteCompleted _ -> fields
        |> upsert_int_field "started_at_us" (Int.max 0 (elapsed_us - duration_us))
        |> upsert_int_field "completed_at_us" elapsed_us
        | Riot_build.Summary _ -> fields
        |> upsert_int_field "started_at_us" 0
        |> upsert_int_field "completed_at_us" elapsed_us
        | Riot_build.NoSuitesFound _ -> upsert_int_field "completed_at_us" elapsed_us fields
        | Riot_build.Build _ -> fields
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ~command_started_at ~duration_us event (json: Data.Json.t) ->
  print (Data.Json.to_string (stamp_json_event ~command_started_at ~duration_us event json));
  print "\n"

let summary_duration_us = fun ~command_started_at (event: Riot_build.test_event) ->
  match event with
  | Riot_build.Summary _ -> Some (Time.Instant.elapsed command_started_at |> Time.Duration.to_micros)
  | _ -> None

let write_test_event_json = fun ~command_started_at ?(pending_suite = None) (
  event: Riot_build.test_event
) ->
  match event with
  | Riot_build.RunningSuite suite ->
      Some (Some suite)
  | Riot_build.SuiteCompleted { summary; _ } ->
      if summary.total > 0 then
        (
          pending_suite
          |> Option.iter
            (fun suite ->
              Riot_build.test_event_to_json (Riot_build.RunningSuite suite)
              |> Option.iter
                (fun json ->
                  write_json_event
                    ~command_started_at
                    ~duration_us:None (Riot_build.RunningSuite suite)
                    json));
          Riot_build.test_event_to_json event
          |> Option.iter
            (fun json ->
              write_json_event
                ~command_started_at
                ~duration_us:(summary_duration_us ~command_started_at event)
                event
                json);
          Some None
        )
      else
        Some None
  | _ ->
      Riot_build.test_event_to_json event
      |> Option.iter
        (fun json ->
          write_json_event
            ~command_started_at
            ~duration_us:(summary_duration_us ~command_started_at event)
            event
            json);
      Some None

let find_suite_source_path = fun ~(workspace:Riot_model.Workspace.t) (suite: Riot_build.suite_binary) ->
  workspace.packages |> List.find_map
    (fun (pkg: Riot_model.Package.t) ->
      if String.equal pkg.name suite.package_name then
        pkg.binaries |> List.find_map
          (fun (bin: Riot_model.Package.binary) ->
            if String.equal bin.name suite.suite_name then
              Some Path.(pkg.path / bin.path)
            else
              None)
      else
        None)

let suite_source_label = fun ~(workspace:Riot_model.Workspace.t) (suite: Riot_build.suite_binary) ->
  match find_suite_source_path ~workspace suite with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Path.to_string relative_path
      | Error _ -> Path.to_string path
    )
  | None -> suite.package_name ^ "/" ^ suite.suite_name

let print_suite_header = fun ~(workspace:Riot_model.Workspace.t) (suite: Riot_build.suite_binary) total ->
  println "";
  println ("     Running " ^ suite_source_label ~workspace suite);
  println "";
  println ("running " ^ Int.to_string total ^ " tests")

let print_test_result = fun (result: Riot_build.test_case_result) ->
  let prefix =
    match result.test_type with
    | Riot_build.Test -> "test"
    | Riot_build.Property _ -> "prop"
  in
  let metadata = metadata_suffix result.size result.reliability in
  match result.result with
  | Riot_build.Passed ->
      let suffix =
        match result.test_type with
        | Riot_build.Test -> "ok"
        | Riot_build.Property { examples } -> Int.to_string examples ^ " examples ok"
      in
      println
        (prefix ^ " " ^ result.name ^ metadata ^ " ... " ^ suffix ^ attempts_suffix result.attempts)
  | Riot_build.Failed message ->
      println
        (prefix ^ " " ^ result.name ^ metadata ^ " ... FAILED" ^ attempts_suffix result.attempts);
      if not (String.equal message "") then
        println ("       " ^ message)
  | Riot_build.Timed_out { timeout_ms } ->
      println
        (prefix
        ^ " "
        ^ result.name
        ^ metadata
        ^ " ... TIMED OUT "
        ^ timeout_message timeout_ms
        ^ attempts_suffix result.attempts)
  | Riot_build.Skipped ->
      println (prefix ^ " " ^ result.name ^ metadata ^ " ... skipped")

let print_suite_footer = fun (summary: Riot_build.test_suite_summary) ->
  println "";
  let status =
    if summary.failed > 0 then
      "FAILED"
    else
      "ok"
  in
  println
    ("test result: "
    ^ status
    ^ ". "
    ^ Int.to_string summary.passed
    ^ " passed; "
    ^ Int.to_string summary.failed
    ^ " failed; "
    ^ Int.to_string summary.skipped
    ^ " skipped")

let print_suite_results = fun ~(workspace:Riot_model.Workspace.t) ~verbose ~(suite:Riot_build.suite_binary) ~stdout ~stderr (
  summary: Riot_build.test_suite_summary
) ->
  if summary.total > 0 then
    (
      print_suite_header ~workspace suite summary.total;
      summary.results |> List.iter print_test_result;
      print_suite_footer summary;
      if verbose > 0 then
        print_command_output Command.{ stdout; stderr; status = 0 }
    )

let write_test_event = fun ~(workspace:Riot_model.Workspace.t) ~(timing:timing_summary) ~verbose (
  event: Riot_build.test_event
) ->
  match event with
  | Riot_build.Build _ -> ()
  | Riot_build.NoSuitesFound { package_name; suite_name } -> print_empty_hint package_name suite_name
  | Riot_build.RunningSuite _ -> ()
  | Riot_build.SuiteCompleted {
    suite;
    stdout;
    stderr;
    summary;
    _
  } ->
      if summary.total > 0 then
        record_suite_timing timing ~suite_label:(suite_source_label ~workspace suite) summary;
      print_suite_results ~workspace ~verbose ~suite ~stdout ~stderr summary
  | Riot_build.Summary {
    total;
    passed;
    failed;
    skipped;
    failed_tests=_
  } -> print_summary ~label:"Test Summary:" ~total ~passed ~failed ~skipped ~timing

let write_test_error = fun err -> println ("error: " ^ Riot_build.test_error_message err)

let write_test_error_json = fun ~command_started_at err ->
  let event_json = Data.Json.Object [
    ("type", Data.Json.String "test.error");
    ("message", Data.Json.String (Riot_build.test_error_message err));
  ] in
  print
    (
      Data.Json.to_string
        (
          match event_json with
          | Data.Json.Object fields -> Data.Json.Object (upsert_int_field
            "completed_at_us"
            (event_elapsed_us ~command_started_at)
            fields)
          | other -> other
        )
    );
  print "\n"

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let trailing = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let output_mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let small_only = ArgParser.get_flag matches "small" in
  let large_only = ArgParser.get_flag matches "large" in
  let flaky_only = ArgParser.get_flag matches "flaky" in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let profile = profile_of_matches matches in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  if small_only && large_only then
    Error (Failure "Cannot combine --small and --large")
  else
    match Riot_model.Workspace_operational_config.load ~workspace_root:workspace.root with
    | Error err ->
        let message = Riot_model.Workspace_operational_config.message err in
        (
          match output_mode with
          | Build.Json ->
              print
                (Data.Json.to_string
                  (Data.Json.Object [
                    ("type", Data.Json.String "test.error");
                    ("message", Data.Json.String message);
                    ("completed_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
                  ]));
              print "\n"
          | Build.Human -> println ("error: " ^ message)
        );
        Error (Failure message)
    | Ok operational_config ->
        let size_filter =
          if small_only then
            Test_selection.Small
          else if large_only then
            Test_selection.Large
          else
            Test_selection.All
        in
        let request = Test_selection.parse_request ~pattern ~legacy_package ~size_filter ~flaky_only in
        let extra_args = Test_selection.extra_args
          ~small_test_timeout:operational_config.test.small_test_timeout
          ~flaky_max_retries:operational_config.test.flaky_max_retries
          request
          trailing in
        let pending_json_suite = ref None in
        let timing = empty_timing_summary () in
        let on_event (event: Riot_build.test_event) =
          match event with
          | Riot_build.Build build_event -> (
              match output_mode with
              | Build.Json -> Build.write_build_event_json build_event
              | Build.Human -> (
                  match build_event with
                  | Riot_build.Pm kind -> Build.write_pm_event
                    ~mode:output_mode
                    ~seen_registry_updates
                    kind
                  | Riot_build.BuildingTarget { target; host } -> Build.write_building_target_event
                    ~mode:output_mode
                    ~target
                    ~host
                  | Riot_build.CacheGc event -> Build.write_cache_gc_event ~mode:output_mode event
                  | Riot_build.Streaming streaming_event -> Build.write_streaming_event
                    ~mode:output_mode
                    ~displayed_packages
                    ~progress
                    streaming_event
                )
            )
          | _ -> (
              match output_mode with
              | Build.Json -> pending_json_suite := write_test_event_json
                ~command_started_at
                ~pending_suite:!pending_json_suite
                event
              |> Option.unwrap_or ~default:None
              | Build.Human -> write_test_event ~workspace ~timing ~verbose event
            )
        in
        match
          Riot_build.test ~on_event
            {
              workspace;
              package_filter = request.package_filter;
              suite_filter = request.suite_filter;
              profile;
              extra_args;
            }
        with
        | Ok () -> Ok ()
        | Error err ->
            (
              match output_mode with
              | Build.Json -> write_test_error_json ~command_started_at err
              | Build.Human -> write_test_error err
            );
            Error (Failure (Riot_build.test_error_message err))
