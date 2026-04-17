open Global
open Collections
open Arg_parser

let test_type_fields = function
  | Test_case.UnitTest -> [ ("type", Data.Json.String "test") ]
  | Test_case.Property { examples } -> [
    ("type", Data.Json.String "property");
    ("examples", Data.Json.Int examples);
  ]

let event_test_type_fields = function
  | Test_case.UnitTest -> [ ("test_type", Data.Json.String "test") ]
  | Test_case.Property { examples } -> [
    ("test_type", Data.Json.String "property");
    ("examples", Data.Json.Int examples);
  ]

let size_to_json = function
  | Test_case.Small -> Data.Json.String "small"
  | Test_case.Large -> Data.Json.String "large"

let reliability_fields = function
  | Test_case.Stable -> [ ("reliability", Data.Json.String "stable") ]
  | Test_case.Flaky { retry_attempts } -> [
    ("reliability", Data.Json.String "flaky");
    ("retry_attempts", Data.Json.Int retry_attempts);
  ]

let descriptor_fields = fun (test: Runner.test_descriptor) ->
  [
    ("index", Data.Json.Int test.index);
    ("name", Data.Json.String test.name);
    ("size", size_to_json test.size);
  ]
  @ event_test_type_fields test.test_type
  @ reliability_fields test.reliability

let snapshot_mode_to_json = function
  | Test_context.External -> Data.Json.String "external"
  | Test_context.Inline -> Data.Json.String "inline"

let snapshot_format_to_json = function
  | Test_context.Text -> Data.Json.String "text"
  | Test_context.Json -> Data.Json.String "json"

let snapshot_reason_to_json = function
  | Test_context.Missing_approved -> Data.Json.String "missing_approved"
  | Test_context.Pending_exists -> Data.Json.String "pending_exists"
  | Test_context.Mismatch -> Data.Json.String "mismatch"

let optional_path_fields = fun name value ->
  match value with
  | Some path -> [ (name, Data.Json.String (Path.to_string path)) ]
  | None -> []

let progress_fields = function
  | Test_context.PropertyIterationPassed { current; total; size } -> [
    ("progress_type", Data.Json.String "property_iteration_passed");
    ("current", Data.Json.Int current);
    ("total", Data.Json.Int total);
    ("size_value", Data.Json.Int size);
  ]
  | Test_context.PropertyAssumptionRejected { current; total; size; rejected_count } -> [
    ("progress_type", Data.Json.String "property_assumption_rejected");
    ("current", Data.Json.Int current);
    ("total", Data.Json.Int total);
    ("size_value", Data.Json.Int size);
    ("rejected_count", Data.Json.Int rejected_count);
  ]
  | Test_context.PropertyCounterExampleFound { current; total; size } -> [
    ("progress_type", Data.Json.String "property_counter_example_found");
    ("current", Data.Json.Int current);
    ("total", Data.Json.Int total);
    ("size_value", Data.Json.Int size);
  ]
  | Test_context.PropertyShrinkStep { current; total; step; max_steps } -> [
    ("progress_type", Data.Json.String "property_shrink_step");
    ("current", Data.Json.Int current);
    ("total", Data.Json.Int total);
    ("step", Data.Json.Int step);
    ("max_steps", Data.Json.Int max_steps);
  ]
  | Test_context.SnapshotAssertionStarted { mode; format; approved_path; pending_path } ->
      [
        ("progress_type", Data.Json.String "snapshot_assertion_started");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
      ]
      @ optional_path_fields "approved_path" approved_path
      @ optional_path_fields "pending_path" pending_path
  | Test_context.SnapshotAssertionMatched { mode; format; approved_path } ->
      [
        ("progress_type", Data.Json.String "snapshot_assertion_matched");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
      ]
      @ optional_path_fields "approved_path" approved_path
  | Test_context.SnapshotAssertionMismatch { mode; format; approved_path; pending_path; reason } ->
      [
        ("progress_type", Data.Json.String "snapshot_assertion_mismatch");
        ("snapshot_mode", snapshot_mode_to_json mode);
        ("snapshot_format", snapshot_format_to_json format);
        ("reason", snapshot_reason_to_json reason);
      ]
      @ optional_path_fields "approved_path" approved_path
      @ optional_path_fields "pending_path" pending_path

let single_result_fields = function
  | Test_result.Passed -> [ ("status", Data.Json.String "passed") ]
  | Test_result.Skipped -> [ ("status", Data.Json.String "skipped") ]
  | Test_result.Failed message -> [
    ("status", Data.Json.String "failed");
    ("message", Data.Json.String message);
  ]
  | Test_result.Timed_out { timeout } -> [
    ("status", Data.Json.String "timed_out");
    ("timeout_ms", Data.Json.Int (Time.Duration.to_millis timeout));
  ]

let event_started_at = ref None

let event_elapsed_us = fun () ->
  match !event_started_at with
  | Some started_at -> Time.Instant.elapsed started_at |> Time.Duration.to_micros
  | None -> 0

let write_json_line = fun json ->
  print (Data.Json.to_string json);
  print "\n"

let event_to_json = function
  | Runner.SuiteStarted { suite_name; total } ->
      event_started_at := Some (Time.Instant.now ());
      Data.Json.Object [
        ("type", Data.Json.String "TestSuiteStarted");
        ("suite", Data.Json.String suite_name);
        ("total", Data.Json.Int total);
        ("started_at_us", Data.Json.Int 0);
      ]
  | Runner.TestStarted test ->
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseStarted");
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ descriptor_fields test)
  | Runner.TestProgress { test; attempt; progress } ->
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseProgress");
            ("attempt", Data.Json.Int attempt);
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ descriptor_fields test
        @ progress_fields progress)
  | Runner.TestAttemptStarted { test; attempt; timeout } ->
      let timeout_fields =
        match timeout with
        | Some timeout -> [ ("timeout_ms", Data.Json.Int (Time.Duration.to_millis timeout)) ]
        | None -> []
      in
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseAttemptStarted");
            ("attempt", Data.Json.Int attempt);
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ descriptor_fields test
        @ timeout_fields)
  | Runner.TestHeartbeat { test; attempt; elapsed } ->
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseHeartbeat");
            ("attempt", Data.Json.Int attempt);
            ("elapsed_us", Data.Json.Int (Time.Duration.to_micros elapsed));
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ descriptor_fields test)
  | Runner.TestAttemptFinished { test; attempt; result; duration } ->
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseAttemptFinished");
            ("attempt", Data.Json.Int attempt);
            ("duration_us", Data.Json.Int (Time.Duration.to_micros duration));
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ descriptor_fields test
        @ single_result_fields result)
  | Runner.TestFinished result ->
      Data.Json.Object
        ([
            ("type", Data.Json.String "TestCaseCompleted");
            ("index", Data.Json.Int result.index);
            ("name", Data.Json.String result.name);
            ("attempts", Data.Json.Int result.attempts);
            ("duration_us", Data.Json.Int (Time.Duration.to_micros result.duration));
            ("emitted_at_us", Data.Json.Int (event_elapsed_us ()));
          ]
        @ event_test_type_fields result.test_type
        @ [ ("size", size_to_json result.size) ]
        @ reliability_fields result.reliability
        @ single_result_fields result.result)

let json_event_handler = fun event ->
  event_to_json event |> write_json_line

let matches_query = fun query (test: Test_case.t) ->
  match query with
  | None -> true
  | Some query -> String.contains test.name query

let matches_size = fun ~small_only ~large_only (test: Test_case.t) ->
  match (small_only, large_only, test.size) with
  | (false, false, _)
  | (true, false, Test_case.Small)
  | (false, true, Test_case.Large) -> true
  | _ -> false

let matches_flaky = fun ~flaky_only (test: Test_case.t) ->
  not flaky_only || match test.reliability with
  | Test_case.Stable -> false
  | Test_case.Flaky _ -> true

let filtered_tests = fun ~query ~small_only ~large_only ~flaky_only tests ->
  List.filter
    tests
    ~fn:(fun (test: Test_case.t) ->
      matches_query query test
      && matches_size ~small_only ~large_only test
      && matches_flaky ~flaky_only test)

let write_tests_json = fun tests ->
  let rec to_json_items index = function
    | [] -> []
    | (test: Test_case.t) :: rest ->
        let base_fields = [
          ("index", Data.Json.Int index);
          ("name", Data.Json.String test.name);
          ("size", size_to_json test.size);
          ("skip", Data.Json.Bool test.skip);
        ] in
        Data.Json.Object (base_fields @ test_type_fields test.test_type @ reliability_fields test.reliability)
        :: to_json_items (index + 1) rest
  in
  let tests_json = to_json_items 1 tests in
  print (Data.Json.to_string (Data.Json.Object [ ("tests", Data.Json.Array tests_json) ]));
  print "\n"

let list_tests = fun ~json tests ->
  if json then
    write_tests_json tests
  else
    List.for_each tests ~fn:(fun (test: Test_case.t) -> println test.name);
  Ok ()

let parse_format_to_reporter = function
  | "tap" -> Ok (module Reporter.TAP : Reporter.Intf)
  | "json" -> Ok (module Reporter.JSON : Reporter.Intf)
  | "junit" -> Ok (module Reporter.JUnit : Reporter.Intf)
  | "pretty" -> Ok (module Reporter.Pretty : Reporter.Intf)
  | "minimal" -> Ok (module Reporter.Minimal : Reporter.Intf)
  | other -> Error ("Unknown format: " ^ other)

let run_tests_cmd =
  let open Arg in
    command "run-tests" |> about "Run tests that match query" |> args
      [
        positional "query" |> required false |> help "Test name substring to filter by";
        flag "json" |> long "json" |> help "Emit machine-readable JSON output";
        option "format"
        |> long "format"
        |> help "Output format: tap, json, junit, pretty, minimal"
        |> default "pretty"
        |> possible_values [ "tap"; "json"; "junit"; "pretty"; "minimal" ];
        flag "shuffle" |> long "shuffle" |> help "Run tests in random order";
        option "concurrency"
        |> long "concurrency"
        |> help "Number of concurrent workers"
        |> default "1";
        flag "small" |> long "small" |> help "Run only tests marked small";
        flag "large" |> long "large" |> help "Run only tests marked large";
        flag "flaky" |> long "flaky" |> help "Run only tests marked flaky";
        option "small-timeout-ms" |> long "small-timeout-ms" |> help "Timeout to apply to tests marked small";
        option "flaky-max-retries" |> long "flaky-max-retries" |> help "Retry budget for tests marked flaky";
        option "pattern" |> long "pattern" |> help "Deprecated alias for the positional query argument";
      ]

let list_tests_cmd =
  let open Arg in command "list-tests"
  |> about "List all tests"
  |> args
    [
      positional "query" |> required false |> help "Test name substring to filter by";
      flag "json" |> long "json" |> help "Emit machine-readable JSON output";
      flag "small" |> long "small" |> help "List only tests marked small";
      flag "large" |> long "large" |> help "List only tests marked large";
      flag "flaky" |> long "flaky" |> help "List only tests marked flaky";
      option "pattern" |> long "pattern" |> help "Deprecated alias for the positional query argument";
    ]

let get_suite_info name: Reporter.suite_info =
  let binary_path = Env.args |> List.head |> Option.unwrap_or ~default:name |> Path.v in
  { name; source_file = None; binary_path = Some binary_path }

let main = fun ~name ~tests ~args ->
  let suite_info = get_suite_info name in
  let cmd = command name
  |> about ("Test runner for " ^ name)
  |> subcommands [ list_tests_cmd; run_tests_cmd ] in
  match get_matches cmd args with
  | Error err ->
      print_error err;
      Error (Failure (error_message err))
  | Ok matches -> (
      match get_subcommand matches with
      | Some ("list-tests", sub_matches) ->
          let small_only = get_flag sub_matches "small" in
          let large_only = get_flag sub_matches "large" in
          if small_only && large_only then
            Error (Failure "Cannot combine --small and --large")
          else
            let flaky_only = get_flag sub_matches "flaky" in
            let query =
              match get_one sub_matches "query" with
              | Some query -> Some query
              | None -> get_one sub_matches "pattern"
            in
            filtered_tests ~query ~small_only ~large_only ~flaky_only tests
            |> list_tests ~json:(get_flag sub_matches "json")
      | Some ("run-tests", sub_matches) -> (
          let format_str =
            if get_flag sub_matches "json" then
              "json"
            else
              get_one sub_matches "format" |> Option.unwrap_or ~default:"pretty"
          in
          match parse_format_to_reporter format_str with
          | Error msg ->
              println ("Error: " ^ msg);
              Error (Failure msg)
          | Ok reporter ->
              let shuffle = get_flag sub_matches "shuffle" in
              let concurrency = get_int sub_matches "concurrency" |> Option.unwrap_or ~default:1 in
              let small_only = get_flag sub_matches "small" in
              let large_only = get_flag sub_matches "large" in
              let flaky_only = get_flag sub_matches "flaky" in
              let query =
                match get_one sub_matches "query" with
                | Some query -> Some query
                | None -> get_one sub_matches "pattern"
              in
              if small_only && large_only then
                Error (Failure "Cannot combine --small and --large")
              else
                let size_filter =
                  if small_only then
                    Runner.Only_small
                  else if large_only then
                    Runner.Only_large
                  else
                    Runner.All_sizes
                in
                let small_test_timeout = get_int sub_matches "small-timeout-ms"
                |> Option.map ~fn:Time.Duration.from_millis in
                let flaky_max_retries = get_int sub_matches "flaky-max-retries"
                |> Option.unwrap_or ~default:0 in
                let target = Runner.{ query; size_filter; flaky_only } in
                let mode =
                  if shuffle then
                    Runner.Shuffle
                  else
                    Runner.Sequential
                in
                let config =
                  Runner.{
                    concurrency;
                    reporter;
                    mode;
                    target;
                    policy = { small_test_timeout; flaky_max_retries };
                    suite_info;
                    event_handler =
                      if String.equal format_str "json" then
                        json_event_handler
                      else
                        Runner.no_event_handler;
                  }
                in
                let summary = Runner.run_tests ~config tests in
                if summary.failed > 0 then
                  System.exit 1;
                Ok ()
        )
      | _ ->
          let reporter =
            (module Reporter.Pretty : Reporter.Intf)
          in
          let config =
            Runner.{
              concurrency = 1;
              reporter;
              mode = Sequential;
              target = { query = None; size_filter = All_sizes; flaky_only = false };
              policy = default_policy;
              suite_info;
              event_handler = Runner.no_event_handler;
            }
          in
          let summary = Runner.run_tests ~config tests in
          if summary.failed > 0 then
            System.exit 1;
          Ok ()
    )
