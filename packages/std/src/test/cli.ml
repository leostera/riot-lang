open Global
open Collections
open Arg_parser

let list_tests = fun tests ->
  List.iter (fun (test: Test_case.t) -> println test.name) tests;
  Ok ()

let parse_format_to_reporter = function
  | "tap" -> Ok (module Reporter.TAP : Reporter.Intf)
  | "json" -> Ok (module Reporter.JSON : Reporter.Intf)
  | "junit" -> Ok (module Reporter.JUnit : Reporter.Intf)
  | "pretty" -> Ok (module Reporter.Pretty : Reporter.Intf)
  | "minimal" -> Ok (module Reporter.Minimal : Reporter.Intf)
  | other -> Error ("Unknown format: " ^ other)

let run_tests_cmd =
  let open Arg in command "run-tests"
  |> about "Run tests that match query"
  |> args
    [
      positional "query" |> required false |> help "Test name substring to filter by";
      flag "json" |> long "json" |> help "Emit machine-readable JSON output";
      option "format"
      |> long "format"
      |> help "Output format: tap, json, junit, pretty, minimal"
      |> default "pretty"
      |> possible_values [ "tap"; "json"; "junit"; "pretty"; "minimal" ];
      flag "shuffle" |> long "shuffle" |> help "Run tests in random order";
      option "concurrency" |> long "concurrency" |> help "Number of concurrent workers" |> default "1";
      flag "small" |> long "small" |> help "Run only tests marked small";
      flag "large" |> long "large" |> help "Run only tests marked large";
      flag "flaky" |> long "flaky" |> help "Run only tests marked flaky";
      option "small-timeout-ms"
      |> long "small-timeout-ms"
      |> help "Timeout to apply to tests marked small";
      option "flaky-max-retries"
      |> long "flaky-max-retries"
      |> help "Retry budget for tests marked flaky";
      option "pattern" |> long "pattern" |> help "Deprecated alias for the positional query argument";
    ]

let list_tests_cmd = command "list-tests" |> about "List all tests"

let get_suite_info name: Reporter.suite_info =
  let binary_path = List.hd Env.args |> Path.v in
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
      | Some ("list-tests", _) ->
          list_tests tests
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
                let small_test_timeout =
                  get_int sub_matches "small-timeout-ms"
                  |> Option.map Time.Duration.from_millis
                in
                let flaky_max_retries =
                  get_int sub_matches "flaky-max-retries"
                  |> Option.unwrap_or ~default:0
                in
                let target =
                  Runner.{
                    query;
                    size_filter;
                    flaky_only;
                  }
                in
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
                  }
                in
                let summary = Runner.run_tests ~config tests in
                if summary.failed > 0 then
                  exit 1;
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
              target = {
                query = None;
                size_filter = All_sizes;
                flaky_only = false;
              };
              policy = default_policy;
              suite_info;
            }
          in
          let summary = Runner.run_tests ~config tests in
          if summary.failed > 0 then
            exit 1;
          Ok ()
    )
