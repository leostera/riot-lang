open Std
open Tusk_model
open ArgParser

type suite_binary = {
  package_name: string;
  suite_name: string;
}

let reconnect = fun ~workspace -> Local_session.connect_local ~workspace () |> Result.expect ~msg:"Failed to start local tusk session"

let command =
  let open ArgParser in
    let open Arg in
      command "bench"
      |> about "Run benchmarks with optional substring matching"
      |> ArgParser.allow_trailing_args
      |> args
        [ positional "pattern" |> required false |> help
            "Benchmark query passed to every benchmark suite binary. Use \
               -p/--package to limit execution to one package. Omit to run all \
               benchmarks."; option "package" |> short 'p' |> long "package" |> help "Run benchmarks from a specific package"; flag
            "verbose"
          |> short 'v'
          |> long "verbose"
          |> help "Enable verbose output for benchmarks"
          |> count; ]

let trailing_args = fun matches ->
  let args = ArgParser.trailing_args matches in
  match args with
  | "--" :: rest -> rest
  | _ -> args

let is_benchmark_binary_name = fun name ->
  String.ends_with ~suffix:"_bench" name || String.ends_with ~suffix:"-bench" name

let compare_suite_binary = fun left right ->
  String.compare
    (left.package_name ^ ":" ^ left.suite_name)
    (right.package_name ^ ":" ^ right.suite_name)

let collect_suite_binaries = fun (workspace: Workspace.t) ?package_filter () ->
  workspace.packages |> List.filter Package.is_workspace_member |> List.filter
    (fun (pkg: Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal pkg.name package_name) |> List.concat_map
    (fun (pkg: Package.t) ->
      List.filter_map
        (fun (bin: Package.binary) ->
          if is_benchmark_binary_name bin.name then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None)
        pkg.binaries) |> List.sort compare_suite_binary

let find_suite_binary_path = fun client (suite: suite_binary) ->
  Local_session.find_artifact client ~package:suite.package_name ~kind:"binary" ~name:suite.suite_name

let run_suite_binary_capture = fun ~extra_args binary_path ->
  let cmd = Command.make binary_path ~args:(("run-benchmarks" :: extra_args)) in
  Command.output cmd

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_run_label = fun (suite: suite_binary) ->
  println "";
  println ("Running " ^ suite.package_name ^ "/" ^ suite.suite_name ^ "...");
  println ""

let print_empty_hint = fun package_filter ->
  match package_filter with
  | Some package_name -> println ("No benchmark suites found in package '" ^ package_name ^ "'")
  | None -> println "No benchmark binaries found"

let print_summary = fun ~total ~passed ~failed ->
  println "";
  println "Benchmark Summary:";
  println ("  Total benchmark suites: " ^ Int.to_string total);
  println ("  Passed: " ^ Int.to_string passed);
  println ("  Failed: " ^ Int.to_string failed)

let run_suite = fun client ~extra_args query (suite: suite_binary) ->
  match find_suite_binary_path client suite with
  | Error msg ->
      println ("error: " ^ msg);
      `Failed
  | Ok binary_path ->
      let bench_args =
        match query with
        | None -> extra_args
        | Some query -> query :: extra_args
      in
      match run_suite_binary_capture ~extra_args:bench_args binary_path with
      | Ok output ->
          print_run_label suite;
          print_command_output output;
          if Int.equal output.status 0 then
            `Passed
          else
            `Failed
      | Error (Command.SystemError msg) ->
          println ("error: " ^ msg);
          `Failed

let run_all_suites = fun ~workspace ~extra_args ~package_filter ~query ->
  let suite_binaries = collect_suite_binaries workspace ?package_filter () in
  if suite_binaries = [] then
    (
      print_empty_hint package_filter;
      Ok ()
    )
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
              total := !total + 1;
              match run_suite client ~extra_args query suite with
              | `Passed -> passed := !passed + 1
              | `Failed -> failed := !failed + 1)
            suite_binaries;
          print_summary ~total:!total ~passed:!passed ~failed:!failed;
          if !failed > 0 then
            Error (Failure (Int.to_string !failed ^ " benchmark suite(s) failed"))
          else
            Ok ()
        in
        Local_session.close client;
        result

let run = fun matches ->
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get current directory" in
  let (workspace, _load_errors) = Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
  let request = Test_selection.parse_request ~pattern ~legacy_package in
  run_all_suites ~workspace ~extra_args ~package_filter:request.package_filter ~query:request.query
