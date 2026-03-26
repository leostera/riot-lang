open Std
open Tusk_model
open ArgParser

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

let split_lines output =
  output
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))

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

let list_test_cases binary_path =
  let cmd = Command.make binary_path ~args:[ "list-tests" ] in
  match Command.output cmd with
  | Ok output when Int.equal output.status 0 -> Ok (split_lines output.stdout)
  | Ok output ->
      Error
        ("list-tests exited "
        ^ Int.to_string output.status
        ^ " for "
        ^ binary_path)
  | Error (Command.SystemError msg) ->
      Error ("failed to list tests from " ^ binary_path ^ ": " ^ msg)

let discover_suite client (suite : suite_binary) =
  match find_suite_binary_path client suite with
  | Error msg ->
      Error
        ("failed to locate test binary "
        ^ suite.package_name
        ^ "/"
        ^ suite.suite_name
        ^ ": "
        ^ msg)
  | Ok binary_path -> (
      match list_test_cases binary_path with
      | Ok case_names ->
          Ok
            Test_selection.
              {
                package_name = suite.package_name;
                suite_name = suite.suite_name;
                case_names;
              }
      | Error msg -> Error msg)

let rec discover_suites client suites discovered =
  match suites with
  | [] -> Ok (List.rev discovered)
  | suite :: rest -> (
      match discover_suite client suite with
      | Ok discovered_suite -> discover_suites client rest (discovered_suite :: discovered)
      | Error _ as err -> err)

let run_suite_binary ~extra_args binary_path =
  let cmd = Command.make binary_path ~args:("run-tests" :: extra_args) in
  Command.status cmd

let run_selection client ~extra_args selection =
  let suite_name =
    match selection with
    | Test_selection.RunSuite suite -> suite
    | Test_selection.RunCases { suite; _ } -> suite
  in
  match
    find_suite_binary_path client
      { package_name = suite_name.package_name; suite_name = suite_name.suite_name }
  with
  | Error msg ->
      println ("error: " ^ msg);
      `Failed
  | Ok binary_path ->
      let test_args =
        match selection with
        | Test_selection.RunSuite _ -> extra_args
        | Test_selection.RunCases { query; _ } -> "--pattern" :: query :: extra_args
      in
      let run_label =
        match selection with
        | Test_selection.RunSuite suite ->
            suite.package_name ^ "/" ^ suite.suite_name
        | Test_selection.RunCases { suite; matched_cases; _ } ->
            suite.package_name
            ^ "/"
            ^ suite.suite_name
            ^ " ("
            ^ Int.to_string (List.length matched_cases)
            ^ " matching case(s))"
      in
      println "";
      println ("Running " ^ run_label ^ "...");
      println "";
      match run_suite_binary ~extra_args:test_args binary_path with
      | Ok 0 -> `Passed
      | Ok _ -> `Failed
      | Error (Command.SystemError msg) ->
          println ("error: " ^ msg);
          `Failed

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
                match
                  run_selection client ~extra_args
                    (Test_selection.RunSuite
                       Test_selection.
                         {
                           package_name = suite.package_name;
                           suite_name = suite.suite_name;
                           case_names = [];
                         })
                with
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
          match discover_suites client suite_binaries [] with
          | Error msg ->
              println ("error: " ^ msg);
              Error (Failure "Failed to discover tests")
          | Ok discovered_suites ->
              let selections = Test_selection.select request discovered_suites in
              if selections = [] then (
                print_query_empty_hint request;
                Ok ())
              else
                let total = List.length selections in
                let passed = ref 0 in
                let failed = ref 0 in
                List.iter
                  (fun selection ->
                    match run_selection client ~extra_args selection with
                    | `Passed -> passed := !passed + 1
                    | `Failed -> failed := !failed + 1)
                  selections;
                print_summary ~label:"Test Summary:" ~total ~passed:!passed
                  ~failed:!failed;
                if !failed > 0 then
                  Error (Failure (Int.to_string !failed ^ " test suite(s) failed"))
                else Ok ()
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
