open Std
open Riot_model
open Riot_build
open ArgParser

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

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_run_label = fun (suite: Riot_build.suite_binary) ->
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

let write_bench_event = fun (event: Riot_build.bench_event) ->
  match event with
  | Riot_build.Build _ -> ()
  | Riot_build.NoSuitesFound { package_name } -> print_empty_hint package_name
  | Riot_build.RunningSuite suite -> print_run_label suite
  | Riot_build.SuiteCompleted { stdout; stderr; _ } -> print_command_output
    Command.{ stdout; stderr; status = 0 }
  | Riot_build.Summary { total; passed; failed } -> print_summary ~total ~passed ~failed

let write_bench_error = fun err -> println ("error: " ^ Riot_build.bench_error_message err)

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
  let on_event (event: Riot_build.bench_event) =
    match event with
    | Riot_build.Build build_event -> (
        match build_event with
        | Riot_build.Pm kind -> Build.write_pm_event ~mode:Build.Human ~seen_registry_updates kind
        | Riot_build.BuildingTarget { target; host } -> Build.write_building_target_event
          ~mode:Build.Human
          ~target
          ~host
        | Riot_build.CacheGc event -> Build.write_cache_gc_event ~mode:Build.Human event
        | Riot_build.Streaming streaming_event -> Build.write_streaming_event
          ~mode:Build.Human
          ~displayed_packages
          ~progress
          streaming_event
      )
    | _ -> write_bench_event event
  in
  match Riot_build.bench
    ~on_event
    { workspace; package_filter = request.package_filter; query = request.query; extra_args } with
  | Ok () -> Ok ()
  | Error err ->
      write_bench_error err;
      Error (Failure (Riot_build.bench_error_message err))
