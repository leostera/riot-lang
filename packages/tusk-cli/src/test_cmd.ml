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

let write_test_event = fun (event: Tusk_build.test_event) ->
  match event with
  | Tusk_build.Build _ -> ()
  | Tusk_build.NoSuitesFound { package_name } -> print_empty_hint package_name
  | Tusk_build.RunningSuite suite -> print_run_label suite
  | Tusk_build.SuiteCompleted { stdout; stderr; _ } -> print_command_output
    Command.{ stdout; stderr; status = 0 }
  | Tusk_build.Summary { total; passed; failed } -> print_summary
    ~label:"Test Summary:"
    ~total
    ~passed
    ~failed

let write_test_error = fun err -> println ("error: " ^ Tusk_build.test_error_message err)

let run = fun ~workspace matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let request = Test_selection.parse_request ~pattern ~legacy_package in
  let on_event (event: Tusk_build.test_event) =
    match event with
    | Tusk_build.Build build_event -> (
        match build_event with
        | Tusk_build.Pm kind -> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates kind
        | Tusk_build.BuildingTarget { target; host } -> Build.write_building_target_event
          ~mode:Build.Human
          ~target
          ~host
        | Tusk_build.Streaming streaming_event -> Build.write_streaming_event
          ~mode:Build.Human
          ~displayed_packages
          ~progress
          streaming_event
      )
    | _ -> write_test_event event
  in
  match Tusk_build.test
    ~on_event
    { workspace; package_filter = request.package_filter; query = request.query; extra_args } with
  | Ok () -> Ok ()
  | Error err ->
      write_test_error err;
      Error (Failure (Tusk_build.test_error_message err))
