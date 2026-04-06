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
            "json"
          |> long "json"
          |> help "Emit machine-readable JSONL events"; flag "verbose"
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

let print_duration = fun duration -> Time.Duration.to_secs_string ~precision:6 duration

let print_bench_result = fun (result: Riot_build.bench_case_result) ->
  match result.result with
  | Riot_build.Completed stats ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ":");
      println ("  iterations: " ^ Int.to_string stats.iterations);
      println ("  mean:       " ^ print_duration stats.mean);
      println ("  median:     " ^ print_duration stats.median);
      println ("  min:        " ^ print_duration stats.min);
      println ("  max:        " ^ print_duration stats.max);
      println ("  std_dev:    " ^ print_duration stats.std_dev);
      println ""
  | Riot_build.Skipped ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ": SKIPPED");
      println ""
  | Riot_build.Failed message ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ": FAILED");
      println ("  Error: " ^ message);
      println ""

let print_comparison = fun (result: Riot_build.bench_comparison_result) ->
  println ("Comparison: " ^ result.description);
  println ("  Fastest: " ^ result.fastest);
  result.case_results |> List.iter
    (fun (case_result: Riot_build.bench_comparison_case_result) ->
      let stats = case_result.statistics in
      println ("  " ^ case_result.name ^ ":");
      println ("    iterations: " ^ Int.to_string stats.iterations);
      println
        ("    mean:       " ^ print_duration stats.mean ^ " ± " ^ print_duration stats.std_dev);
      println ("    min:        " ^ print_duration stats.min);
      println ("    max:        " ^ print_duration stats.max));
  if not (result.speedup_ratios = []) then
    (
      println "  Relative speed:";
      result.speedup_ratios |> List.iter
        (fun (name, ratio) ->
          if not (String.equal name result.fastest) then
            println
              ("    "
              ^ result.fastest
              ^ " ran "
              ^ Float.to_string ~precision:2 ratio
              ^ "x faster than "
              ^ name))
    );
  println ""

let print_summary = fun ~total ~completed ~skipped ~failed ->
  println "";
  println "Benchmark Summary:";
  println ("  Total benchmarks: " ^ Int.to_string total);
  println ("  Completed: " ^ Int.to_string completed);
  println ("  Skipped: " ^ Int.to_string skipped);
  println ("  Failed: " ^ Int.to_string failed)

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at |> Time.Duration.to_micros

let json_int_field = fun name fields ->
  match List.assoc_opt name fields with
  | Some (Data.Json.Int value) -> Some value
  | _ -> None

let upsert_int_field = fun name value fields ->
  let filtered =
    List.filter (fun (field_name, _) -> not (String.equal field_name name)) fields
  in
  filtered @ [ (name, Data.Json.Int value) ]

let stamp_json_event = fun ~command_started_at ~duration_us (event: Riot_build.bench_event) (
  json: Data.Json.t
) ->
  match json with
  | Data.Json.Object fields ->
      let elapsed_us = event_elapsed_us ~command_started_at in
      let duration_us =
        match duration_us with
        | Some duration_us -> duration_us
        | None -> Option.unwrap_or ~default:0 (json_int_field "duration_us" fields)
      in
      let fields = upsert_int_field "duration_us" duration_us fields in
      let fields =
        match event with
        | Riot_build.RunningSuite _ -> upsert_int_field "started_at_us" elapsed_us fields
        | Riot_build.SuiteCompleted _ -> fields
        |> upsert_int_field "started_at_us" (Int.max 0 (elapsed_us - duration_us))
        |> upsert_int_field "completed_at_us" elapsed_us
        | Riot_build.Summary _ -> fields
        |> upsert_int_field "started_at_us" 0
        |> upsert_int_field "completed_at_us" elapsed_us
        | Riot_build.NoSuitesFound _ -> upsert_int_field "completed_at_us" elapsed_us fields
        | Riot_build.Build _ -> fields
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ~command_started_at ~duration_us event (json: Data.Json.t) ->
  print (Data.Json.to_string (stamp_json_event ~command_started_at ~duration_us event json));
  print "\n"

let summary_duration_us = fun ~command_started_at (event: Riot_build.bench_event) ->
  match event with
  | Riot_build.Summary _ -> Some (Time.Instant.elapsed command_started_at |> Time.Duration.to_micros)
  | _ -> None

let write_bench_event = fun (event: Riot_build.bench_event) ->
  match event with
  | Riot_build.Build _ ->
      ()
  | Riot_build.NoSuitesFound { package_name } ->
      print_empty_hint package_name
  | Riot_build.RunningSuite _ ->
      ()
  | Riot_build.SuiteCompleted {
    suite;
    stdout;
    stderr;
    results;
    comparisons;
    _
  } ->
      let should_print_suite =
        not (results = [])
        || not (comparisons = [])
        || not (String.equal stdout "")
        || not (String.equal stderr "") in
      if should_print_suite then
        (
          print_run_label suite;
          List.iter print_bench_result results;
          List.iter print_comparison comparisons;
          print_command_output Command.{ stdout; stderr; status = 0 }
        )
  | Riot_build.Summary { total; completed; skipped; failed } ->
      print_summary ~total ~completed ~skipped ~failed

let write_bench_error = fun err -> println ("error: " ^ Riot_build.bench_error_message err)

let write_bench_error_json = fun ~command_started_at err ->
  let event_json = Data.Json.Object [
    ("type", Data.Json.String "bench.error");
    ("message", Data.Json.String (Riot_build.bench_error_message err));
  ] in
  print
    (
      Data.Json.to_string
        (
          match event_json with
          | Data.Json.Object fields -> Data.Json.Object (upsert_int_field
            "completed_at_us"
            (event_elapsed_us ~command_started_at)
            fields)
          | other -> other
        )
    );
  print "\n"

let run = fun ~workspace matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let displayed_packages = Collections.HashSet.create () in
  let progress = Build.{ built_count = 0; cached_count = 0; failed_count = 0; skipped_count = 0 } in
  let extra_args = trailing_args matches in
  let verbose = ArgParser.get_count matches "verbose" in
  let _ = verbose in
  let output_mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let request = Test_selection.parse_request ~pattern ~legacy_package in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  let on_event (event: Riot_build.bench_event) =
    match event with
    | Riot_build.Build build_event -> (
        match output_mode with
        | Build.Json -> Build.write_build_event_json build_event
        | Build.Human -> (
            match build_event with
            | Riot_build.Pm kind -> Build.write_pm_event ~mode:output_mode ~seen_registry_updates kind
            | Riot_build.BuildingTarget { target; host } -> Build.write_building_target_event
              ~mode:output_mode
              ~target
              ~host
            | Riot_build.CacheGc event -> Build.write_cache_gc_event ~mode:output_mode event
            | Riot_build.Streaming streaming_event -> Build.write_streaming_event
              ~mode:output_mode
              ~displayed_packages
              ~progress
              streaming_event
          )
      )
    | _ -> (
        match output_mode with
        | Build.Json -> Riot_build.bench_event_to_json event
        |> Option.iter
          (fun json ->
            write_json_event
              ~command_started_at
              ~duration_us:(summary_duration_us ~command_started_at event)
              event
              json)
        | Build.Human -> write_bench_event event
      )
  in
  match Riot_build.bench
    ~on_event
    { workspace; package_filter = request.package_filter; query = request.query; extra_args } with
  | Ok () -> Ok ()
  | Error err ->
      (
        match output_mode with
        | Build.Json -> write_bench_error_json ~command_started_at err
        | Build.Human -> write_bench_error err
      );
      Error (Failure (Riot_build.bench_error_message err))
