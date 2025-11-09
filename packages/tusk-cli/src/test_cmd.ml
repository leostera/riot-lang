open Std
open Std.Collections
open Std.Sync.Cell
open Tusk_model
open Tusk_model
open ArgParser

let command =
  let open ArgParser in
  let open Arg in
  command "test" |> about "Run tests with optional pattern matching"
  |> ArgParser.allow_trailing_args
  |> args
       [
         positional "pattern" |> required false
         |> help
              "Test pattern: 'prefix' runs all tests starting with prefix, \
               'pkg:prefix' runs package-scoped tests. Examples: 'parser_', \
               'tty:api_'. Omit to run all tests.";
         option "package" |> short 'p' |> long "package"
         |> help "Run tests from specific package (deprecated, use pattern)";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for tests"
         |> count;
       ]

let trailing_args matches =
  let args = ArgParser.trailing_args matches in
  match args with "--" :: rest -> rest | _ -> args

let run matches =
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in

  (* Parse pattern: [pkg][:test_prefix] *)
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let (workspace, _load_errors) =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Tusk_server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  (* Parse pattern: [pkg]:test_prefix or pkg:test_prefix *)
  let (package_filter, test_prefix) =
    match (pattern, legacy_package) with
    | (Some p, _) -> (
        match String.split_on_char ':' p with
        | [ pkg; prefix ] -> (Some pkg, Some prefix)
        | [ single ] -> (None, Some single) (* Treat as test prefix across all packages *)
        | _ -> (None, None))
    | (None, Some pkg) -> (Some pkg, None)
    | (None, None) -> (None, None)
  in

  let packages =
    match package_filter with
    | Some pkg_name ->
        List.filter
          (fun (pkg : Package.t) -> String.equal pkg.name pkg_name)
          workspace.packages
    | None -> workspace.packages
  in

  let test_binaries =
    List.concat_map
      (fun (pkg : Package.t) ->
        List.filter_map
          (fun (bin : Package.binary) ->
            let is_test =
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name
            in
            let matches_prefix =
              match test_prefix with
              | None -> true
              | Some prefix -> String.starts_with ~prefix bin.name
            in
            if is_test && matches_prefix then Some (pkg.name, bin.name)
            else None)
          pkg.binaries)
      packages
  in

  if List.length test_binaries = 0 then (
    Tusk_client.close client;
    println "No test binaries found";
    (match (package_filter, test_prefix) with
    | (Some pkg, Some prefix) ->
        println ("Hint: No tests matching '" ^ pkg ^ ":*" ^ prefix ^ "*' found")
    | (Some pkg, None) ->
        println
          ("Hint: Make sure package '" ^ pkg ^ "' has binaries ending in '_tests' or \
           '-tests'")
    | (None, Some prefix) -> println ("Hint: No tests matching '*" ^ prefix ^ "*' found")
    | (None, None) -> println "Hint: Test binaries should end in '_tests' or '-tests'");
    Ok ())
  else
    (* Group test binaries by package *)
    let tests_by_package =
      List.fold_left
        (fun acc (pkg, test_name) ->
          let existing =
            HashMap.get acc pkg |> Option.unwrap_or ~default:[]
          in
          let _ = HashMap.insert acc pkg (test_name :: existing) in
          acc)
        (HashMap.create ()) test_binaries
    in

    let total = List.length test_binaries in
    let failed = ref 0 in
    let passed = ref 0 in

    (* Build each package once, then run all its tests *)
    HashMap.iter
      (fun pkg test_names ->
        println "";
        println ("Building package '" ^ pkg ^ "'...");
        match Build.build_command (Some pkg) with
        | Ok () ->
            List.iter
              (fun test_name ->
                match
                  Tusk_client.find_artifact client ~package:pkg ~kind:"binary"
                    ~name:test_name
                with
                | Ok path -> (
                    let test_args =
                      match extra_args with
                      | [] -> [ "run-tests" ]
                      | _ -> "run-tests" :: extra_args
                    in
                    let cmd = Command.make path ~args:test_args in
                    println "";
                    println ("Running " ^ pkg ^ "/" ^ test_name ^ "...");
                    println "";
                    match Command.status cmd with
                    | Ok 0 -> incr passed
                    | Ok _code -> incr failed
                    | Error (Command.SystemError msg) ->
                        println ("error: " ^ msg);
                        incr failed)
                | Error msg ->
                    println ("error: " ^ msg);
                    incr failed)
              test_names
        | Error _ ->
            println ("error: build failed for package '" ^ pkg ^ "'");
            (* Mark all tests in this package as failed *)
            failed := !failed + List.length test_names)
      tests_by_package;

    Tusk_client.close client;

    println "";
    println "Test Summary:";
    println ("  Total test suites: " ^ Int.to_string total);
    println ("  Passed: " ^ Int.to_string !passed);
    println ("  Failed: " ^ Int.to_string !failed);

    if !failed > 0 then
      Error (Failure (Int.to_string !failed ^ " test suite(s) failed"))
    else Ok ()
