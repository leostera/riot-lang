open Std
open Tusk_model
open Tusk_build
open ArgParser

let command =
  let open ArgParser in
    let open Arg in
      command "test"
      |> about "Run tests with optional substring matching"
      |> ArgParser.allow_trailing_args
      |> args
        [ positional "pattern" |> required false |> help
            "Test query passed to every test suite binary. Use -p/--package \
               to limit execution to one package. Omit to run all tests."; option "package"
          |> short 'p'
          |> long "package"
          |> help "Run tests from a specific package"; flag "verbose"
          |> short 'v'
          |> long "verbose"
          |> help "Enable verbose output for tests"
          |> count; ]

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_run_label = fun (suite: Tusk_build.suite_binary) ->
  println "";
  println ("Running " ^ suite.package_name ^ "/" ^ suite.suite_name ^ "...");
  println ""

let print_empty_hint = fun package_filter ->
  match package_filter with
  | Some package_name -> println ("No test suites found in package '" ^ package_name ^ "'")
  | None -> println "No test binaries found"

let print_summary = fun ~label ~total ~passed ~failed ->
  println "";
  println label;
  println ("  Total test suites: " ^ Int.to_string total);
  println ("  Passed: " ^ Int.to_string passed);
  println ("  Failed: " ^ Int.to_string failed)

let write_test_event = function
  | Tusk_build.Build _ ->
      ()
  | Tusk_build.NoSuitesFound { package_name } ->
      print_empty_hint package_name
  | Tusk_build.RunningSuite suite ->
      print_run_label suite
  | Tusk_build.SuiteCompleted { stdout; stderr; _ } ->
      print_command_output Command.{ stdout; stderr; status = 0 }
  | Tusk_build.Summary { total; passed; failed } ->
      print_summary ~label:"Test Summary:" ~total ~passed ~failed

let write_test_error = fun err ->
  println ("error: " ^ Tusk_build.test_error_message err)

let run = fun matches ->
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
  let (workspace, load_errors) = Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
  let request = Test_selection.parse_request ~pattern ~legacy_package in
  match
    Tusk_build.test
      ~on_event:write_test_event
      {
        workspace;
        load_errors;
        package_filter = request.package_filter;
        query = request.query;
        extra_args;
      }
  with
  | Ok () ->
      Ok ()
  | Error err ->
      write_test_error err;
      Error (Failure (Tusk_build.test_error_message err))
