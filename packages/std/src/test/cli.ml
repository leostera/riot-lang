open Global
open Collections
open Arg_parser

let list_tests tests =
  List.iter (fun (test : Test_case.t) -> println test.name) tests;
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
  command "run-tests"
  |> about "Run tests that match pattern"
  |> args
       [
         option "format" |> long "format"
         |> help "Output format: tap, json, junit, pretty, minimal"
         |> default "pretty"
         |> possible_values [ "tap"; "json"; "junit"; "pretty"; "minimal" ];
         flag "shuffle" |> long "shuffle" |> help "Run tests in random order";
         option "concurrency" |> long "concurrency"
         |> help "Number of concurrent workers"
         |> default "1";
         option "pattern" |> long "pattern"
         |> help "Test name prefix to filter by";
       ]

let list_tests_cmd = command "list-tests" |> about "List all tests"

let get_suite_info name : Reporter.suite_info =
  let binary_path = List.hd Env.args in
  { name; source_file = None; binary_path = Some binary_path }

let main ~name ~tests ~args =
  let suite_info = get_suite_info name in
  let cmd =
    command name
    |> about ("Test runner for " ^ name)
    |> subcommands [ list_tests_cmd; run_tests_cmd ]
  in

  match get_matches cmd args with
  | Error err ->
      print_error err;
      Error (Failure (error_message err))
  | Ok matches -> (
      match get_subcommand matches with
      | Some ("list-tests", _) -> list_tests tests
      | Some ("run-tests", sub_matches) -> (
          let format_str =
            get_one sub_matches "format" |> Option.unwrap_or ~default:"pretty"
          in
          match parse_format_to_reporter format_str with
          | Error msg ->
              println ("Error: " ^ msg);
              Error (Failure msg)
          | Ok reporter ->
              let shuffle = get_flag sub_matches "shuffle" in
              let concurrency =
                get_int sub_matches "concurrency" |> Option.unwrap_or ~default:1
              in
              let target =
                match get_one sub_matches "pattern" with
                | None -> Runner.All
                | Some prefix -> Runner.(FilterByPrefix prefix)
              in
              let mode =
                if shuffle then Runner.Shuffle else Runner.Sequential
              in
              let config =
                Runner.{ concurrency; reporter; mode; target; suite_info }
              in

              let summary = Runner.run_tests ~config tests in
              if summary.failed > 0 then exit 1;
              Ok ())
      | _ ->
          let reporter = (module Reporter.Pretty : Reporter.Intf) in
          let config =
            Runner.
              {
                concurrency = 1;
                reporter;
                mode = Sequential;
                target = All;
                suite_info;
              }
          in
          let summary = Runner.run_tests ~config tests in
          if summary.failed > 0 then exit 1;
          Ok ())
