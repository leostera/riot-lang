open Std
open Core
open Model
open ArgParser

let command =
  let open ArgParser in
  let open Arg in
  command "test" |> about "Run tests" |> ArgParser.allow_trailing_args
  |> args
       [
         option "package" |> short 'p' |> long "package"
         |> help "Run tests only from this package";
         flag "verbose" |> short 'v' |> long "verbose"
         |> help "Enable verbose output for tests"
         |> count;
       ]

let trailing_args matches =
  let args = ArgParser.trailing_args matches in
  match args with "--" :: rest -> rest | _ -> args

let run matches =
  let package_filter = ArgParser.get_one matches "package" in
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let client =
    Server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
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
            if
              String.ends_with ~suffix:"_tests" bin.name
              || String.ends_with ~suffix:"-tests" bin.name
            then Some (pkg.name, bin.name)
            else None)
          pkg.binaries)
      packages
  in

  if List.length test_binaries = 0 then (
    Server.Tusk_jsonrpc.Client.close client;
    println "No test binaries found";
    (match package_filter with
    | Some pkg ->
        println
          "Hint: Make sure package '%s' has binaries ending in '_tests' or \
           '-tests'"
          pkg
    | None -> println "Hint: Test binaries should end in '_tests' or '-tests'");
    Ok ())
  else
    let total = List.length test_binaries in
    let failed = ref 0 in
    let passed = ref 0 in

    List.iter
      (fun (pkg, test_name) ->
        match Build.build_command (Some pkg) with
        | Ok () -> (
            match
              Server.Tusk_jsonrpc.Client.find_artifact client ~package:pkg
                ~kind:"binary" ~name:test_name
            with
            | Ok path -> (
                let test_args =
                  match extra_args with
                  | [] -> [ "run-tests" ]
                  | _ -> "run-tests" :: extra_args
                in
                let cmd = Command.make path ~args:test_args in
                println "";
                println "Running %s/%s..." pkg test_name;
                println "";
                match Command.status cmd with
                | Ok 0 -> incr passed
                | Ok _code -> incr failed
                | Error (Command.SystemError msg) ->
                    println "error: %s" msg;
                    incr failed)
            | Error msg ->
                println "error: %s" msg;
                incr failed)
        | Error _ ->
            println "error: build failed for package '%s'" pkg;
            incr failed)
      test_binaries;

    Server.Tusk_jsonrpc.Client.close client;

    println "";
    println "Test Summary:";
    println "  Total test suites: %d" total;
    println "  Passed: %d" !passed;
    println "  Failed: %d" !failed;

    if !failed > 0 then
      Error (Failure (format "%d test suite(s) failed" !failed))
    else Ok ()
