open Std
open Tusk_model
open ArgParser

let no_matching_tests_exit_code = 3

type suite_binary = {
  package_name : string;
  suite_name : string;
}

let reconnect ~workspace =
  Local_session.connect_local ~workspace
  |> Result.expect ~msg:"Failed to start local tusk session"

let command =
  let open ArgParser in
  let open Arg in
  command "test" |> about "Run tests with optional substring matching"
  |> ArgParser.allow_trailing_args
  |> args
       [
         positional "pattern" |> required false
         |> help
              "Test query: matches package names, test suite names, and test \
               case names by substring. Use 'pkg:query' to scope matching to a \
               package, or 'pkg:...' to run every test suite in a package. \
               Omit to run all test suites.";
         option "package" |> short 'p' |> long "package"
         |> help "Run tests from a specific package (deprecated; use 'pkg:...')";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for tests"
         |> count;
       ]

let trailing_args matches =
  let args = ArgParser.trailing_args matches in
  match args with "--" :: rest -> rest | _ -> args

let is_test_binary_name name =
  String.ends_with ~suffix:"_tests" name || String.ends_with ~suffix:"-tests" name

let compare_suite_binary left right =
  String.compare
    (left.package_name ^ ":" ^ left.suite_name)
    (right.package_name ^ ":" ^ right.suite_name)

let collect_suite_binaries (workspace : Workspace.t) ?package_filter () =
  workspace.packages
  |> List.filter Package.is_workspace_member
  |> List.filter (fun (pkg : Package.t) ->
         match package_filter with
         | None -> true
         | Some package_name -> String.equal pkg.name package_name)
  |> List.concat_map (fun (pkg : Package.t) ->
         List.filter_map
           (fun (bin : Package.binary) ->
             if is_test_binary_name bin.name
             then Some { package_name = pkg.name; suite_name = bin.name }
             else None)
           pkg.binaries)
  |> List.sort compare_suite_binary

let find_suite_binary_path client (suite : suite_binary) =
  Local_session.find_artifact client ~package:suite.package_name ~kind:"binary"
    ~name:suite.suite_name

let run_suite_binary ~extra_args binary_path =
  let cmd = Command.make binary_path ~args:("run-tests" :: extra_args) in
  Command.status cmd

let run_suite_binary_capture ~extra_args binary_path =
  let cmd = Command.make binary_path ~args:("run-tests" :: extra_args) in
  Command.output cmd

let print_command_output (output : Command.output) =
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_run_label (suite : suite_binary) =
  println "";
  println ("Running " ^ suite.package_name ^ "/" ^ suite.suite_name ^ "...");
  println ""

let run_suite client ~extra_args (suite : suite_binary) =
  match find_suite_binary_path client suite with
  | Error msg ->
      println ("error: " ^ msg);
      `Failed
  | Ok binary_path ->
      print_run_label suite;
      match run_suite_binary ~extra_args binary_path with
      | Ok 0 -> `Passed
      | Ok _ -> `Failed
      | Error (Command.SystemError msg) ->
          println ("error: " ^ msg);
          `Failed

let run_query_suite client ~extra_args request (suite : suite_binary) =
  let selection =
    Test_selection.execution_for_suite request
      Test_selection.
        {
          package_name = suite.package_name;
          suite_name = suite.suite_name;
        }
  in
  match selection with
  | None -> `NoMatch
  | Some execution -> (
      match find_suite_binary_path client suite with
      | Error msg ->
          println ("error: " ^ msg);
          `Failed
      | Ok binary_path ->
          let test_args =
            match execution with
            | Test_selection.RunSuite -> extra_args
            | Test_selection.RunQuery query -> query :: extra_args
          in
          match run_suite_binary_capture ~extra_args:test_args binary_path with
          | Ok output when Int.equal output.status no_matching_tests_exit_code ->
              `NoMatch
          | Ok output ->
              print_run_label suite;
              print_command_output output;
              if Int.equal output.status 0 then `Passed else `Failed
          | Error (Command.SystemError msg) ->
              println ("error: " ^ msg);
              `Failed)

let print_fast_path_empty_hint package_filter =
  println "No test binaries found";
  match package_filter with
  | Some package_name ->
      println
        ("Hint: Make sure package '"
        ^ package_name
        ^ "' has binaries ending in '_tests' or '-tests'")
  | None -> println "Hint: Test binaries should end in '_tests' or '-tests'"

let print_query_empty_hint request =
  match request with
  | Test_selection.Query query ->
      println ("No tests matched '" ^ query ^ "'")
  | Test_selection.PackageAll package_name ->
      println ("No test suites found in package '" ^ package_name ^ "'")
  | Test_selection.PackageQuery { package_name; query } ->
      println ("No tests matched '" ^ package_name ^ ":" ^ query ^ "'")
  | Test_selection.All ->
      println "No test binaries found"

let print_summary ~label ~total ~passed ~failed =
  println "";
  println label;
  println ("  Total test suites: " ^ Int.to_string total);
  println ("  Passed: " ^ Int.to_string passed);
  println ("  Failed: " ^ Int.to_string failed)

let run_fast_path ~workspace ~extra_args ~package_filter =
  let suite_binaries = collect_suite_binaries workspace ?package_filter () in
  if suite_binaries = [] then (
    print_fast_path_empty_hint package_filter;
    Ok ())
  else
    let packages =
      suite_binaries
      |> List.map (fun (suite : suite_binary) -> suite.package_name)
      |> List.sort_uniq String.compare
    in
    let total = List.length suite_binaries in
    let passed = ref 0 in
    let failed = ref 0 in
    List.iter
      (fun package_name ->
        let package_suites =
          List.filter
            (fun (suite : suite_binary) ->
              String.equal suite.package_name package_name)
            suite_binaries
        in
        println "";
        println ("Building package '" ^ package_name ^ "'...");
        match Build.build_command ~scope:Build.Dev (Some package_name) None with
        | Ok () ->
            let client = reconnect ~workspace in
            List.iter
              (fun (suite : suite_binary) ->
                match run_suite client ~extra_args suite with
                | `Passed -> passed := !passed + 1
                | `Failed -> failed := !failed + 1)
              package_suites;
            Local_session.close client
        | Error _ ->
            println ("error: build failed for package '" ^ package_name ^ "'");
            failed := !failed + List.length package_suites)
      packages;
    print_summary ~label:"Test Summary:" ~total ~passed:!passed ~failed:!failed;
    if !failed > 0 then
      Error (Failure (Int.to_string !failed ^ " test suite(s) failed"))
    else Ok ()

let run_query_path ~workspace ~extra_args request =
  let package_filter = Test_selection.package_filter request in
  let suite_binaries = collect_suite_binaries workspace ?package_filter () in
  if suite_binaries = [] then (
    print_query_empty_hint request;
    Ok ())
  else
    match Build.build_command ~scope:Build.Dev None None with
    | Error _ -> Error (Failure "Build failed")
    | Ok () ->
        let client = reconnect ~workspace in
        let result =
          let total = ref 0 in
          let passed = ref 0 in
          let failed = ref 0 in
          List.iter
            (fun suite ->
              match run_query_suite client ~extra_args request suite with
              | `NoMatch -> ()
              | `Passed ->
                  total := !total + 1;
                  passed := !passed + 1
              | `Failed ->
                  total := !total + 1;
                  failed := !failed + 1)
            suite_binaries;
          if !total = 0 then (
            print_query_empty_hint request;
            Ok ())
          else (
            print_summary ~label:"Test Summary:" ~total:!total ~passed:!passed
              ~failed:!failed;
            if !failed > 0 then
              Error (Failure (Int.to_string !failed ^ " test suite(s) failed"))
            else Ok ())
        in
        Local_session.close client;
        result

let run matches =
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  match pattern with
  | None -> run_fast_path ~workspace ~extra_args ~package_filter:legacy_package
  | Some _ ->
      let request = Test_selection.parse_request ~pattern ~legacy_package in
      run_query_path ~workspace ~extra_args request
