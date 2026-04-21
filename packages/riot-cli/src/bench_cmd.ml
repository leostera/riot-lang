open Std
open Std.Result.Syntax
open Riot_model
open Riot_build
open ArgParser
open Riot_bench

let command =
  let open ArgParser in
    let open Arg in command "bench"
    |> about "Run benchmarks with optional case filtering"
    |> ArgParser.allow_trailing_args
    |> args
      [
        option "package" |> short 'p' |> long "package" |> multiple |> help "Run benchmarks from a specific package. Repeat to run multiple packages.";
        option "filter" |> short 'f' |> long "filter" |> help "Filter benchmark suites and cases by substring within the selected packages";
        flag "list" |> long "list" |> help "List benchmark suites and benchmark cases without running them";
        flag "release" |> long "release" |> help "Use the release build profile";
        flag "json" |> long "json" |> help "Emit machine-readable JSONL events";
        flag "verbose"
        |> short 'v'
        |> long "verbose"
        |> help "Enable verbose output for benchmarks"
        |> count;
      ]

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

let parse_package_names = fun package_names ->
  let rec loop acc = function
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error -> Error (Failure ("invalid package name '" ^ package_name ^ "': " ^ error))
      )
  in
  loop [] package_names

let print_command_output = fun (output: Command.output) ->
  if not (String.equal output.stdout "") then
    print output.stdout;
  if not (String.equal output.stderr "") then
    eprint output.stderr

let print_run_label = fun (suite: Bench_runtime.suite_binary) ->
  println "";
  println ("Running " ^ Package_name.to_string suite.package_name ^ "/" ^ suite.suite_name ^ "...");
  println ""

let print_empty_hint = fun package_filter ->
  match package_filter with
  | Some package_name -> println
    ("No benchmark suites found in package '" ^ Package_name.to_string package_name ^ "'")
  | None -> println "No benchmark binaries found"

let print_empty_list_hint = fun package_filter query ->
  match query with
  | Some query -> println ("No benchmarks matched query '" ^ query ^ "'")
  | None -> print_empty_hint package_filter

let print_duration = fun duration -> Time.Duration.to_secs_string ~precision:6 duration

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at |> Time.Duration.to_micros

let listed_suite_source_label = fun ~(workspace:Riot_model.Workspace.t) (
  suite: Bench_runtime.listed_bench_suite
) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Path.to_string relative_path
      | Error _ -> Path.to_string path
    )
  | None -> Package_name.to_string suite.suite.package_name ^ "/" ^ suite.suite.suite_name

let listed_bench_selector = fun (suite: Bench_runtime.suite_binary) (
  item: Bench_runtime.listed_bench_item
) ->
  Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name ^ ":" ^ item.name

let listed_bench_item_json = fun (suite: Bench_runtime.suite_binary) (
  item: Bench_runtime.listed_bench_item
) ->
  let kind =
    match item.kind with
    | Bench_runtime.Benchmark -> Data.Json.String "benchmark"
    | Bench_runtime.Comparison -> Data.Json.String "comparison"
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
  suite: Bench_runtime.listed_bench_suite
) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Data.Json.String (Path.to_string relative_path)
      | Error _ -> Data.Json.String (Path.to_string path)
    )
  | None -> Data.Json.Null

let listed_suite_selector = fun (suite: Bench_runtime.suite_binary) ->
  Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name

let write_json_line = fun json -> println (Data.Json.to_string json)

let write_bench_suite_listed_json = fun ~command_started_at ~(workspace:Riot_model.Workspace.t) (
  suite: Bench_runtime.listed_bench_suite
) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListed");
      ("package", Data.Json.String (Package_name.to_string suite.suite.package_name));
      ("suite", Data.Json.String suite.suite.suite_name);
      ("path", listed_suite_path_json ~workspace suite);
      ("selector", Data.Json.String (listed_suite_selector suite.suite));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_item_listed_json = fun ~command_started_at (suite: Bench_runtime.suite_binary) (
  item: Bench_runtime.listed_bench_item
) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchItemListed");
      ("package", Data.Json.String (Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("name", Data.Json.String item.name);
      ("selector", Data.Json.String (listed_bench_selector suite item));
      ("benchmark", listed_bench_item_json suite item);
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_suite_list_failed_json = fun ~command_started_at (suite: Bench_runtime.suite_binary) err ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListFailed");
      ("package", Data.Json.String (Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("selector", Data.Json.String (listed_suite_selector suite));
      ("message", Data.Json.String (Bench_runtime.bench_error_message err));
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
  List.for_each suites
    ~fn:(fun (suite: Bench_runtime.listed_bench_suite) ->
      println "";
      println (listed_suite_source_label ~workspace suite);
      suite.benchmarks |> List.for_each
        ~fn:(fun (item: Bench_runtime.listed_bench_item) ->
          let kind =
            match item.kind with
            | Bench_runtime.Benchmark -> "bench"
            | Bench_runtime.Comparison -> "compare"
          in
          let skip_suffix =
            if item.skip then
              " [skip]"
            else
              ""
          in
          println ("  [" ^ Int.to_string item.index ^ "] " ^ kind ^ " " ^ item.name ^ skip_suffix)))

let print_bench_result = fun (result: Bench_runtime.bench_case_result) ->
  match result.result with
  | Bench_runtime.Completed stats ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ":");
      println ("  iterations: " ^ Int.to_string stats.iterations);
      println ("  mean:       " ^ print_duration stats.mean);
      println ("  median:     " ^ print_duration stats.median);
      println ("  min:        " ^ print_duration stats.min);
      println ("  max:        " ^ print_duration stats.max);
      println ("  std_dev:    " ^ print_duration stats.std_dev);
      println ""
  | Bench_runtime.Skipped ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ": SKIPPED");
      println ""
  | Bench_runtime.Failed message ->
      println ("[" ^ Int.to_string result.index ^ "] " ^ result.name ^ ": FAILED");
      println ("  Error: " ^ message);
      println ""

let print_comparison = fun (result: Bench_runtime.bench_comparison_result) ->
  println ("Comparison: " ^ result.description);
  println ("  Fastest: " ^ result.fastest);
  result.case_results |> List.for_each
    ~fn:(fun (case_result: Bench_runtime.bench_comparison_case_result) ->
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
      result.speedup_ratios |> List.for_each
        ~fn:(fun (name, ratio) ->
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

let bench_history_warning = fun message -> eprintln ("warning: " ^ message)

let json_int_field = fun name fields ->
  match
    List.find fields
      ~fn:(fun (field_name, _) ->
        String.equal field_name name)
  with
  | Some (_, Data.Json.Int value) -> Some value
  | _ -> None

let upsert_int_field = fun name value fields ->
  let filtered =
    List.filter fields ~fn:(fun (field_name, _) -> not (String.equal field_name name))
  in
  filtered @ [ (name, Data.Json.Int value) ]

let stamp_json_event = fun ~command_started_at ~duration_us (event: Bench_runtime.bench_event) (
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
        | Bench_runtime.RunningSuite _ -> upsert_int_field "started_at_us" elapsed_us fields
        | Bench_runtime.SuiteCompleted _ -> fields
        |> upsert_int_field "started_at_us" (Int.max 0 (elapsed_us - duration_us))
        |> upsert_int_field "completed_at_us" elapsed_us
        | Bench_runtime.Summary _ -> fields
        |> upsert_int_field "started_at_us" 0
        |> upsert_int_field "completed_at_us" elapsed_us
        | Bench_runtime.NoSuitesFound _ -> upsert_int_field "completed_at_us" elapsed_us fields
        | Bench_runtime.Build _ -> fields
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ~command_started_at ~duration_us event (json: Data.Json.t) ->
  println (Data.Json.to_string (stamp_json_event ~command_started_at ~duration_us event json))

let summary_duration_us = fun ~command_started_at (event: Bench_runtime.bench_event) ->
  match event with
  | Bench_runtime.Summary _ -> Some (Time.Instant.elapsed command_started_at |> Time.Duration.to_micros)
  | _ -> None

let write_bench_event = fun (event: Bench_runtime.bench_event) ->
  match event with
  | Bench_runtime.Build _ ->
      ()
  | Bench_runtime.NoSuitesFound { package_name } ->
      print_empty_hint package_name
  | Bench_runtime.RunningSuite _ ->
      ()
  | Bench_runtime.SuiteCompleted {
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
  | Bench_runtime.Summary { total; completed; skipped; failed } ->
      print_summary ~total ~completed ~skipped ~failed

let write_bench_error = fun err -> println ("error: " ^ Bench_runtime.bench_error_message err)

let write_bench_error_json = fun ~command_started_at err ->
  let event_json = Data.Json.Object [
    ("type", Data.Json.String "bench.error");
    ("message", Data.Json.String (Bench_runtime.bench_error_message err));
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

let bench_history_partial = fun (request: Test_selection.request) ->
  not (List.is_empty request.package_filters)
  || Option.is_some request.suite_filter
  || Option.is_some request.query

let bench_history_context = fun ~(workspace:Riot_model.Workspace.t) ~profile (
  request: Test_selection.request
) ~argv ->
  History.create_run_context
    ~workspace_root:workspace.root
    ~profile
    ~filter:request.query
    ~partial:(bench_history_partial request)
    ~argv
    ()

let bench_history_of_statistics = fun (stats: Bench_runtime.bench_statistics) : History.bench_statistics ->
  {
    min = stats.min;
    max = stats.max;
    mean = stats.mean;
    median = stats.median;
    std_dev = stats.std_dev;
    iterations = stats.iterations;
    total_time = stats.total_time;
  }

let bench_history_of_result = fun (result: Bench_runtime.bench_case_result) : History.bench_case_result ->
  let result_status: History.bench_case_status =
    match result.result with
    | Bench_runtime.Completed stats -> History.Completed (bench_history_of_statistics stats)
    | Bench_runtime.Failed message -> History.Failed message
    | Bench_runtime.Skipped -> History.Skipped
  in
  { index = result.index; name = result.name; result = result_status }

let bench_history_of_comparison = fun (comparison: Bench_runtime.bench_comparison_result) : History.bench_comparison_result ->
  {
    description = comparison.description;
    fastest = comparison.fastest;
    case_results = List.map
      comparison.case_results
      ~fn:(fun (case_result: Bench_runtime.bench_comparison_case_result) ->
        {
          History.name = case_result.name;
          statistics = bench_history_of_statistics case_result.statistics
        });
    speedup_ratios = comparison.speedup_ratios
  }

let save_bench_history = fun context ~(suite:Bench_runtime.suite_binary) status started_at_us completed_at_us duration_us (
  results: Bench_runtime.bench_case_result list
) (comparisons: Bench_runtime.bench_comparison_result list) (
  summary: Bench_runtime.bench_suite_summary
) ->
  let suite_run: History.suite_run = {
    status;
    started_at_us;
    completed_at_us;
    duration_us;
    summary = {
      total = summary.total;
      completed = summary.completed;
      skipped = summary.skipped;
      failed = summary.failed
    };
    benchmarks = List.map results ~fn:bench_history_of_result;
    comparisons = List.map comparisons ~fn:bench_history_of_comparison;
  }
  in
  History.save_suite_run context ~package_name:suite.package_name ~suite_name:suite.suite_name ~suite_run

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let seen_registry_updates = Collections.HashSet.create () in
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
  let pattern = ArgParser.get_one matches "filter" in
  let package_filters = parse_package_names (ArgParser.get_many matches "package") in
  let profile = profile_of_matches matches in
  let* package_filters = package_filters in
  let* request = Test_selection.parse_request
    ~filter:pattern
    ~package_filters
    ~size_filter:Test_selection.All
    ~flaky_only:false
  |> Result.map_err ~fn:(fun error -> Failure error) in
  let extra_args = Test_selection.extra_args request extra_args in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  if list_mode then
    let listed_suite_count = ref 0 in
    let listed_benchmark_count = ref 0 in
    let failed_suite_count = ref 0 in
    let on_suite (suite: Bench_runtime.listed_bench_suite) =
      if not (List.is_empty suite.benchmarks) then
        (
          listed_suite_count := !listed_suite_count + 1;
          listed_benchmark_count := !listed_benchmark_count + List.length suite.benchmarks;
          write_bench_suite_listed_json ~command_started_at ~workspace suite;
          List.for_each
            suite.benchmarks
            ~fn:(write_bench_item_listed_json ~command_started_at suite.suite)
        )
    in
    let on_suite_error (suite: Bench_runtime.suite_binary) err =
      failed_suite_count := !failed_suite_count + 1;
      write_bench_suite_list_failed_json ~command_started_at suite err
    in
    match
      Bench_runtime.list_benchmarks
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
          package_filters = request.package_filters;
          suite_filter = request.suite_filter;
          profile;
          extra_args;
        }
    with
    | Ok suites ->
        let suites =
          List.filter
            suites
            ~fn:(fun (suite: Bench_runtime.listed_bench_suite) ->
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
        Error (Failure (Bench_runtime.bench_error_message err))
  else
    let history_context = bench_history_context ~workspace ~profile request ~argv:Env.args in
    let on_event (event: Bench_runtime.bench_event) =
      match event with
      | Bench_runtime.Build build_event ->
          Build.write_build_event ~mode:output_mode ~seen_registry_updates build_event
      | Bench_runtime.SuiteCompleted {
        suite;
        status;
        started_at_us;
        completed_at_us;
        duration_us;
        results;
        comparisons;
        summary;
        _
      } ->
          save_bench_history
            history_context
            ~suite
            status
            started_at_us
            completed_at_us
            duration_us
            results
            comparisons
            summary
          |> Result.iter_err
            ~fn:(fun error ->
              bench_history_warning
                ("failed to save benchmark history for "
                ^ Package_name.to_string suite.package_name
                ^ "/"
                ^ suite.suite_name
                ^ ": "
                ^ error));
          (
            match output_mode with
            | Build.Json -> Bench_runtime.bench_event_to_json event
            |> Option.for_each
              ~fn:(fun json ->
                write_json_event
                  ~command_started_at
                  ~duration_us:(summary_duration_us ~command_started_at event)
                  event
                  json)
            | Build.Human -> write_bench_event event
          )
      | _ -> (
          match output_mode with
          | Build.Json -> Bench_runtime.bench_event_to_json event
          |> Option.for_each
            ~fn:(fun json ->
              write_json_event
                ~command_started_at
                ~duration_us:(summary_duration_us ~command_started_at event)
                event
                json)
          | Build.Human -> write_bench_event event
        )
    in
    match
      Bench_runtime.bench ~on_event
        {
          workspace;
          package_filters = request.package_filters;
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
        Error (Failure (Bench_runtime.bench_error_message err))
