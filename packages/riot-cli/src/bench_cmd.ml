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
               package:suite or -p/--package to narrow execution. Omit to run \
               all benchmarks."; option "package" |> short 'p' |> long "package" |> help "Run benchmarks from a specific package"; flag
            "list"
          |> long "list"
          |> help "List benchmark suites and benchmark cases without running them"; flag "release"
          |> long "release"
          |> help "Use the release build profile"; flag "json" |> long "json" |> help "Emit machine-readable JSONL events"; flag
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

let profile_of_matches = fun matches ->
  if ArgParser.get_flag matches "release" then
    "release"
  else
    "debug"

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

let print_empty_list_hint = fun package_filter query ->
  match query with
  | Some query -> println ("No benchmarks matched query '" ^ query ^ "'")
  | None -> print_empty_hint package_filter

let print_duration = fun duration -> Time.Duration.to_secs_string ~precision:6 duration

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at |> Time.Duration.to_micros

let listed_suite_source_label = fun ~(workspace:Riot_model.Workspace.t) (
  suite: Riot_build.listed_bench_suite
) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Path.to_string relative_path
      | Error _ -> Path.to_string path
    )
  | None -> suite.suite.package_name ^ "/" ^ suite.suite.suite_name

let listed_bench_selector = fun (suite: Riot_build.suite_binary) (item: Riot_build.listed_bench_item) ->
  suite.package_name ^ ":" ^ suite.suite_name ^ ":" ^ item.name

let listed_bench_item_json = fun (suite: Riot_build.suite_binary) (
  item: Riot_build.listed_bench_item
) ->
  let kind =
    match item.kind with
    | Riot_build.Benchmark -> Data.Json.String "benchmark"
    | Riot_build.Comparison -> Data.Json.String "comparison"
  in
  Data.Json.Object [
    ("index", Data.Json.Int item.index);
    ("name", Data.Json.String item.name);
    ("selector", Data.Json.String (listed_bench_selector suite item));
    ("kind", kind);
    ("iterations", Data.Json.Int item.iterations);
    ("warmup", Data.Json.Int item.warmup);
    ("skip", Data.Json.Bool item.skip);
    ("cases", Data.Json.Array (List.map item.cases ~fn:Data.Json.string));
  ]

let listed_suite_path_json = fun ~(workspace:Riot_model.Workspace.t) (
  suite: Riot_build.listed_bench_suite
) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Data.Json.String (Path.to_string relative_path)
      | Error _ -> Data.Json.String (Path.to_string path)
    )
  | None -> Data.Json.Null

let listed_suite_selector = fun (suite: Riot_build.suite_binary) ->
  suite.package_name ^ ":" ^ suite.suite_name

let write_json_line = fun json ->
  print (Data.Json.to_string json);
  print "\n"

let write_bench_suite_listed_json = fun ~command_started_at ~(workspace:Riot_model.Workspace.t) (
  suite: Riot_build.listed_bench_suite
) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListed");
      ("package", Data.Json.String suite.suite.package_name);
      ("suite", Data.Json.String suite.suite.suite_name);
      ("path", listed_suite_path_json ~workspace suite);
      ("selector", Data.Json.String (listed_suite_selector suite.suite));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_item_listed_json = fun ~command_started_at (suite: Riot_build.suite_binary) (
  item: Riot_build.listed_bench_item
) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchItemListed");
      ("package", Data.Json.String suite.package_name);
      ("suite", Data.Json.String suite.suite_name);
      ("name", Data.Json.String item.name);
      ("selector", Data.Json.String (listed_bench_selector suite item));
      ("benchmark", listed_bench_item_json suite item);
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_suite_list_failed_json = fun ~command_started_at (suite: Riot_build.suite_binary) err ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListFailed");
      ("package", Data.Json.String suite.package_name);
      ("suite", Data.Json.String suite.suite_name);
      ("selector", Data.Json.String (listed_suite_selector suite));
      ("message", Data.Json.String (Riot_build.bench_error_message err));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_list_completed_json = fun ~command_started_at ~suite_count ~benchmark_count ~failed_suite_count ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchListCompleted");
      ("suite_count", Data.Json.Int suite_count);
      ("benchmark_count", Data.Json.Int benchmark_count);
      ("failed_suite_count", Data.Json.Int failed_suite_count);
      ("completed_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_list = fun ~(workspace:Riot_model.Workspace.t) suites ->
  List.for_each suites ~fn:
    (fun (suite: Riot_build.listed_bench_suite) ->
      println "";
      println (listed_suite_source_label ~workspace suite);
      suite.benchmarks |> List.for_each ~fn:
        (fun (item: Riot_build.listed_bench_item) ->
          let kind =
            match item.kind with
            | Riot_build.Benchmark -> "bench"
            | Riot_build.Comparison -> "compare"
          in
          let skip_suffix =
            if item.skip then
              " [skip]"
            else
              ""
          in
          println ("  [" ^ Int.to_string item.index ^ "] " ^ kind ^ " " ^ item.name ^ skip_suffix)))

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
  result.case_results |> List.for_each ~fn:
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
      result.speedup_ratios |> List.for_each ~fn:
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

let json_int_field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
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
          List.for_each results ~fn:print_bench_result;
          List.for_each comparisons ~fn:print_comparison;
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

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
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
  let list_mode = ArgParser.get_flag matches "list" in
  let pattern = ArgParser.get_one matches "pattern" in
  let legacy_package = ArgParser.get_one matches "package" in
  let profile = profile_of_matches matches in
  let request = Test_selection.parse_request
    ~pattern
    ~legacy_package
    ~size_filter:Test_selection.All
    ~flaky_only:false in
  let extra_args = Test_selection.extra_args request extra_args in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  if list_mode then
    let listed_suite_count = ref 0 in
    let listed_benchmark_count = ref 0 in
    let failed_suite_count = ref 0 in
    let on_suite (suite: Riot_build.listed_bench_suite) =
      if not (List.is_empty suite.benchmarks) then
        (
          listed_suite_count := !listed_suite_count + 1;
          listed_benchmark_count := !listed_benchmark_count + List.length suite.benchmarks;
          write_bench_suite_listed_json ~command_started_at ~workspace suite;
          List.for_each suite.benchmarks ~fn:(write_bench_item_listed_json ~command_started_at suite.suite)
        )
    in
    let on_suite_error (suite: Riot_build.suite_binary) err =
      failed_suite_count := !failed_suite_count + 1;
      write_bench_suite_list_failed_json ~command_started_at suite err
    in
    match
      Riot_build.list_benchmarks
        ?on_suite:(
          if output_mode = Build.Json then
            Some on_suite
          else
            None
        )
        ?on_suite_error:(
          if output_mode = Build.Json then
            Some on_suite_error
          else
            None
        )
        {
          workspace;
          package_filter = request.package_filter;
          suite_filter = request.suite_filter;
          profile;
          extra_args;
        }
    with
    | Ok suites ->
        let suites =
          List.filter suites ~fn:(fun (suite: Riot_build.listed_bench_suite) ->
            not (List.is_empty suite.benchmarks))
        in
        (
          match output_mode with
          | Build.Json -> write_bench_list_completed_json
            ~command_started_at
            ~suite_count:!listed_suite_count
            ~benchmark_count:!listed_benchmark_count
            ~failed_suite_count:!failed_suite_count
          | Build.Human ->
              if List.is_empty suites then
                print_empty_list_hint request.package_filter request.query
              else
                write_bench_list ~workspace suites
        );
        Ok ()
    | Error err ->
        (
          match output_mode with
          | Build.Json -> write_bench_error_json ~command_started_at err
          | Build.Human -> write_bench_error err
        );
        Error (Failure (Riot_build.bench_error_message err))
  else
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
          |> Option.for_each ~fn:
            (fun json ->
              write_json_event
                ~command_started_at
                ~duration_us:(summary_duration_us ~command_started_at event)
                event
                json)
          | Build.Human -> write_bench_event event
        )
    in
    match
      Riot_build.bench ~on_event
        {
          workspace;
          package_filter = request.package_filter;
          suite_filter = request.suite_filter;
          profile;
          extra_args;
        }
    with
    | Ok () -> Ok ()
    | Error err ->
        (
          match output_mode with
          | Build.Json -> write_bench_error_json ~command_started_at err
          | Build.Human -> write_bench_error err
        );
        Error (Failure (Riot_build.bench_error_message err))
