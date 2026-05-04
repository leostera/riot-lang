open Std
open Std.Result.Syntax
open Riot_model
open Riot_build
open ArgParser
open Riot_bench

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "bench"
  |> about "Run benchmarks with optional case filtering"
  |> ArgParser.allow_trailing_args
  |> args
    [
      option "package"
      |> short 'p'
      |> long "package"
      |> multiple
      |> help "Run benchmarks from a specific package. Repeat to run multiple packages.";
      option "filter"
      |> short 'f'
      |> long "filter"
      |> help "Filter benchmark suites and cases by substring within the selected packages";
      option "compare"
      |> long "compare"
      |> help "Show up to N previous comparable suite runs alongside the current results";
      option "iterations"
      |> long "iterations"
      |> help "Override iteration count for all matched benchmarks";
      option "warmup"
      |> long "warmup"
      |> help "Override warmup count for all matched benchmarks";
      flag "record"
      |> long "record"
      |> help "Persist this benchmark run into .riot/bench for later comparison";
      flag "list"
      |> long "list"
      |> help "List benchmark suites and benchmark cases without running them";
      flag "release"
      |> long "release"
      |> help "Use the release build profile";
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSONL events";
      flag "verbose"
      |> short 'v'
      |> long "verbose"
      |> help "Enable verbose output for benchmarks"
      |> count;
    ]

type invocation_args = {
  parsed: string list;
  trailing: string list;
}

let is_known_flag_without_value = fun __tmp1 ->
  match __tmp1 with
  | "--list"
  | "--record"
  | "--release"
  | "--json"
  | "-v"
  | "--verbose"
  | "-h"
  | "--help" -> true
  | _ -> false

let is_known_flag_with_value = fun __tmp1 ->
  match __tmp1 with
  | "-p"
  | "--package"
  | "-f"
  | "--filter"
  | "--compare"
  | "--iterations"
  | "--warmup" -> true
  | _ -> false

let looks_like_flag = fun value -> String.starts_with ~prefix:"-" value

let bench_invocation_args = fun argv ->
  let rec loop parsed trailing = fun __tmp1 ->
    match __tmp1 with
    | [] -> { parsed = List.reverse parsed; trailing = List.reverse trailing }
    | "--" :: rest ->
        { parsed = List.reverse parsed; trailing = List.append (List.reverse trailing) rest }
    | arg :: rest when is_known_flag_without_value arg -> loop (arg :: parsed) trailing rest
    | arg :: value :: rest when is_known_flag_with_value arg ->
        loop (value :: arg :: parsed) trailing rest
    | arg :: value :: rest when looks_like_flag arg && not (looks_like_flag value) ->
        loop parsed (value :: arg :: trailing) rest
    | arg :: rest -> loop parsed (arg :: trailing) rest
  in
  match argv with
  | [] -> { parsed = []; trailing = [] }
  | command_name :: rest ->
      let result = loop [ command_name ] [] rest in
      { parsed = result.parsed; trailing = result.trailing }

let extract_bench_argv = fun argv ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | "bench" :: _ as bench_argv -> Some bench_argv
    | _ :: rest -> loop rest
  in
  loop argv

let reparsed_matches = fun matches ->
  match extract_bench_argv Env.args with
  | None -> Ok (matches, trailing_args matches)
  | Some bench_argv ->
      let invocation = bench_invocation_args bench_argv in
      ArgParser.get_matches command invocation.parsed
      |> Result.map ~fn:(fun matches -> (matches, invocation.trailing))
      |> Result.map_err ~fn:(fun err -> Failure (ArgParser.error_message err))

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

let compare_limit_of_matches = fun matches ->
  match ArgParser.get_int matches "compare" with
  | None -> Ok None
  | Some value when Int.(value <= 0) -> Error (Failure "--compare expects a positive integer")
  | Some value -> Ok (Some value)

let bench_override_args = fun matches ->
  let args = ref [] in
  (
    match ArgParser.get_int matches "iterations" with
    | Some value -> args := !args @ [ "--iterations"; Int.to_string value ]
    | None -> ()
  );
  (
    match ArgParser.get_int matches "warmup" with
    | Some value -> args := !args @ [ "--warmup"; Int.to_string value ]
    | None -> ()
  );
  !args

let parse_package_names = fun package_names ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package_name :: rest -> (
        match Riot_model.Package_name.from_string package_name with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error ->
            Error (Failure ("invalid package name '"
            ^ package_name
            ^ "': "
            ^ Riot_model.Package_name.error_message error))
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
  | Some package_name ->
      println ("No benchmark suites found in package '" ^ Package_name.to_string package_name ^ "'")
  | None -> println "No benchmark binaries found"

let print_empty_list_hint = fun package_filter query ->
  match query with
  | Some query -> println ("No benchmarks matched query '" ^ query ^ "'")
  | None -> print_empty_hint package_filter

let print_duration = fun duration -> Time.Duration.to_secs_string ~precision:6 duration

let event_elapsed_us = fun ~command_started_at ->
  Time.Instant.elapsed command_started_at
  |> Time.Duration.to_micros

let listed_suite_source_label = fun
  ~(workspace:Riot_model.Workspace.t) (suite: Bench_runtime.listed_bench_suite) ->
  match suite.source_path with
  | Some path -> (
      match Path.strip_prefix path ~prefix:workspace.root with
      | Ok relative_path -> Path.to_string relative_path
      | Error _ -> Path.to_string path
    )
  | None -> Package_name.to_string suite.suite.package_name ^ "/" ^ suite.suite.suite_name

let listed_bench_selector = fun
  (suite: Bench_runtime.suite_binary) (item: Bench_runtime.listed_bench_item) ->
  Package_name.to_string suite.package_name ^ ":" ^ suite.suite_name ^ ":" ^ item.name

let listed_bench_item_json = fun
  (suite: Bench_runtime.suite_binary) (item: Bench_runtime.listed_bench_item) ->
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

let listed_suite_path_json = fun
  ~(workspace:Riot_model.Workspace.t) (suite: Bench_runtime.listed_bench_suite) ->
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

let write_bench_suite_listed_json = fun
  ~command_started_at
  ~(workspace:Riot_model.Workspace.t)
  (suite: Bench_runtime.listed_bench_suite) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListed");
      ("package", Data.Json.String (Package_name.to_string suite.suite.package_name));
      ("suite", Data.Json.String suite.suite.suite_name);
      ("path", listed_suite_path_json ~workspace suite);
      ("selector", Data.Json.String (listed_suite_selector suite.suite));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_item_listed_json = fun
  ~command_started_at (suite: Bench_runtime.suite_binary) (item: Bench_runtime.listed_bench_item) ->
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

let write_bench_suite_list_failed_json = fun
  ~command_started_at (suite: Bench_runtime.suite_binary) err ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchSuiteListFailed");
      ("package", Data.Json.String (Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("selector", Data.Json.String (listed_suite_selector suite));
      ("message", Data.Json.String (Bench_runtime.bench_error_message err));
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_list_completed_json = fun
  ~command_started_at ~suite_count ~benchmark_count ~failed_suite_count ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchListCompleted");
      ("suite_count", Data.Json.Int suite_count);
      ("benchmark_count", Data.Json.Int benchmark_count);
      ("failed_suite_count", Data.Json.Int failed_suite_count);
      ("completed_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let write_bench_list = fun ~(workspace:Riot_model.Workspace.t) suites ->
  List.for_each
    suites
    ~fn:(fun (suite: Bench_runtime.listed_bench_suite) ->
      println "";
      println (listed_suite_source_label ~workspace suite);
      suite.benchmarks
      |> List.for_each
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

let terminal_profile = Tty.Profile.from_env ()

let styled = fun color_hex text ->
  let color =
    Tty.Color.make color_hex
    |> Tty.Profile.convert terminal_profile
  in
  Tty.Style.default
  |> Tty.Style.fg color
  |> Tty.Style.bold
  |> fun style -> Tty.Style.styled style text

let styled_fastest_case_name = fun name -> styled "#98C379" name

let comparison_case_label = fun ~fastest name ->
  if String.equal name fastest then
    styled_fastest_case_name name
  else
    name

let render_relative_speed_line = fun ~fastest (name, ratio) ->
  "    "
  ^ styled_fastest_case_name fastest
  ^ " ran "
  ^ Float.to_string ~precision:2 ratio
  ^ "x faster than "
  ^ name

let print_gc = fun ~indent (gc: Bench_runtime.bench_gc_stats) ->
  println (indent ^ "gc.minor:   " ^ Int.to_string gc.minor_collections);
  println (indent ^ "gc.major:   " ^ Int.to_string gc.major_collections);
  println (indent ^ "gc.compact: " ^ Int.to_string gc.compactions)

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
      print_gc ~indent:"  " stats.gc;
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
  println ("  Fastest: " ^ styled_fastest_case_name result.fastest);
  result.case_results
  |> List.for_each
    ~fn:(fun (case_result: Bench_runtime.bench_comparison_case_result) ->
      let stats = case_result.statistics in
      println ("  " ^ comparison_case_label ~fastest:result.fastest case_result.name ^ ":");
      println ("    iterations: " ^ Int.to_string stats.iterations);
      println
        ("    mean:       " ^ print_duration stats.mean ^ " ± " ^ print_duration stats.std_dev);
      println ("    min:        " ^ print_duration stats.min);
      println ("    max:        " ^ print_duration stats.max);
      print_gc ~indent:"    " stats.gc);
  if not (result.speedup_ratios = []) then (
    println "  Relative speed:";
    result.speedup_ratios
    |> List.for_each
      ~fn:(fun (name, ratio) ->
        if not (String.equal name result.fastest) then
          println (render_relative_speed_line ~fastest:result.fastest (name, ratio)))
  );
  println ""

let print_summary = fun ~total ~completed ~skipped ~failed ->
  println "";
  println "Benchmark Summary:";
  println ("  Total benchmarks: " ^ Int.to_string total);
  println ("  Completed: " ^ Int.to_string completed);
  println ("  Skipped: " ^ Int.to_string skipped);
  println ("  Failed: " ^ Int.to_string failed)

let metric_specs = [
  ("mean", fun (stats: History.bench_statistics) -> stats.mean);
  ("median", fun stats -> stats.median);
  ("min", fun stats -> stats.min);
  ("max", fun stats -> stats.max);
  ("std_dev", fun stats -> stats.std_dev);
]

let json_of_option = fun value ~some ->
  match value with
  | Some value -> some value
  | None -> Data.Json.Null

let print_percent = fun value -> Float.to_string ~precision:1 (value *. 100.0) ^ "%"

type cv_band =
  | Great
  | Good
  | Meh
  | Bad

let classify_cv = fun value ->
  if Float.compare value 0.02 = Order.LT then
    Great
  else if Float.compare value 0.05 != Order.GT then
    Good
  else if Float.compare value 0.10 != Order.GT then
    Meh
  else
    Bad

let style_cv_text = fun cv_value text ->
  match cv_value with
  | None -> text
  | Some value -> (
      match classify_cv value with
      | Great -> styled "#98C379" text
      | Good -> styled "#61AFEF" text
      | Meh -> styled "#E5C07B" text
      | Bad -> styled "#E06C75" text
    )

let render_cv = fun cv_value ->
  match cv_value with
  | None -> "n/a"
  | Some value -> print_percent value

let styled_stability_label = fun __tmp1 ->
  match __tmp1 with
  | History.Stable -> styled "#98C379" "stable"
  | History.Noisy -> styled "#E5C07B" "noisy"

let delta_percent = fun current previous ->
  let previous_nanos =
    Time.Duration.to_nanos previous
    |> Int64.to_float
  in
  if Float.equal previous_nanos 0.0 then
    None
  else
    let current_nanos =
      Time.Duration.to_nanos current
      |> Int64.to_float
    in
    Some (((current_nanos -. previous_nanos) /. previous_nanos) *. 100.0)

let noise_margin_percent = fun ~current_cv ~baseline_cv ->
  let values =
    [ current_cv; baseline_cv ]
    |> List.filter_map ~fn:(fun value -> value)
    |> List.map ~fn:(fun value -> value *. 100.0)
  in
  match values with
  | [] -> 0.0
  | values ->
      List.fold_left
        values
        ~init:0.0
        ~fn:(fun acc value ->
          if Float.compare value acc = Order.GT then
            value
          else
            acc)

let render_delta_text = fun delta_value ->
  match delta_value with
  | None -> "n/a"
  | Some value ->
      let sign =
        if Float.compare value 0.0 = Order.GT then
          "+"
        else
          ""
      in
      sign ^ Float.to_string ~precision:1 value ^ "%"

let style_delta_cell = fun delta_value ~noise_margin_percent text ->
  match delta_value with
  | None -> text
  | Some value ->
      if Float.compare (Float.abs value) noise_margin_percent != Order.GT then
        styled "#E5C07B" text
      else if Float.compare value 0.0 = Order.LT then
        styled "#98C379" text
      else
        styled "#E06C75" text

let render_signed_int = fun value ->
  let sign =
    if Int.(value > 0) then
      "+"
    else
      ""
  in
  sign ^ Int.to_string value

let style_signed_int_delta = fun value text ->
  if Int.equal value 0 then
    styled "#E5C07B" text
  else if Int.(value < 0) then
    styled "#98C379" text
  else
    styled "#E06C75" text

let float_delta = fun current previous -> Some (current -. previous)

let render_signed_float = fun value ->
  let sign =
    if Float.compare value 0.0 = Order.GT then
      "+"
    else
      ""
  in
  sign ^ Float.to_string ~precision:4 value

let style_signed_float_delta = fun value text ->
  if Float.equal value 0.0 then
    styled "#E5C07B" text
  else if Float.compare value 0.0 = Order.LT then
    styled "#98C379" text
  else
    styled "#E06C75" text

let gc_per_iteration = fun count iterations ->
  if Int.(iterations <= 0) then
    None
  else
    Some (Float.from_int count /. Float.from_int iterations)

let render_gc_rate = fun value ->
  match value with
  | None -> "n/a"
  | Some value -> Float.to_string ~precision:4 value

let gc_metric_specs = [
  ("minor", fun (gc: History.gc_stats) -> gc.minor_collections);
  ("major", fun gc -> gc.major_collections);
  ("compact", fun gc -> gc.compactions);
]

let history_column_label = fun index (sample: History.history_sample) ->
  let base =
    if Int.equal index 0 then
      "prev"
    else
      "prev-" ^ Int.to_string index
  in
  if sample.partial then
    base ^ "*"
  else
    base

let history_table_header = fun
  ~label_width ~column_width ~current_partial (history: History.history_sample list) ->
  let current_label =
    if current_partial then
      "curr*"
    else
      "curr"
  in
  ([ String.pad_right ~width:label_width ' ' "" ]
  @ [
    String.pad_left ~width:column_width ' ' "delta";
    String.pad_left ~width:column_width ' ' current_label;
  ])
  @ (
    history
    |> List.enumerate
    |> List.map
      ~fn:(fun (index, sample) ->
        String.pad_left
          ~width:column_width
          ' '
          (history_column_label index sample))
  )
  |> String.concat " "

let print_history_table = fun
  ~current_partial
  ~baseline
  ~current_cv
  ~baseline_cv
  current
  (history: History.history_sample list) ->
  let label_width = 10 in
  let column_width = 12 in
  let header = history_table_header ~label_width ~column_width ~current_partial history in
  let noise_margin_percent = noise_margin_percent ~current_cv ~baseline_cv in
  println header;
  metric_specs
  |> List.for_each
    ~fn:(fun (label, project) ->
      let current_value = project current in
      let delta_value = delta_percent current_value (project baseline) in
      let delta_cell =
        render_delta_text delta_value
        |> String.pad_left ~width:column_width ' '
        |> style_delta_cell delta_value ~noise_margin_percent
      in
      let values =
        ([ String.pad_right ~width:label_width ' ' label ]
        @ [ delta_cell; String.pad_left ~width:column_width ' ' (print_duration current_value) ])
        @ (
          history
          |> List.map
            ~fn:(fun (sample: History.history_sample) ->
              String.pad_left
                ~width:column_width
                ' '
                (print_duration (project sample.statistics)))
        )
        |> String.concat " "
      in
      println values);
  let current_cv_value = Statistics.coefficient_of_variation current in
  let baseline_cv_value = Statistics.coefficient_of_variation baseline in
  let current_cv =
    render_cv current_cv_value
    |> String.pad_left ~width:column_width ' '
    |> style_cv_text current_cv_value
  in
  let cv_delta =
    match (current_cv_value, baseline_cv_value) with
    | (Some current_cv, Some baseline_cv) when not (Float.equal baseline_cv 0.0) ->
        let change = ((current_cv -. baseline_cv) /. baseline_cv) *. 100.0 in
        let sign =
          if Float.compare change 0.0 = Order.GT then
            "+"
          else
            ""
        in
        sign ^ Float.to_string ~precision:1 change ^ "%"
    | _ -> "n/a"
  in
  let cv_history =
    history
    |> List.map
      ~fn:(fun (sample: History.history_sample) ->
        let value = Statistics.coefficient_of_variation sample.statistics in
        render_cv value
        |> String.pad_left ~width:column_width ' '
        |> style_cv_text value)
    |> String.concat " "
  in
  println
    (
      ([ String.pad_right ~width:label_width ' ' "cv" ]
      @ [ String.pad_left ~width:column_width ' ' cv_delta; current_cv ])
      @ [ cv_history ]
      |> String.concat " "
    )

let print_gc_history_table = fun
  ~current_partial
  ~(baseline:History.bench_statistics)
  (current: History.bench_statistics)
  (history: History.history_sample list) ->
  let label_width = 10 in
  let column_width = 12 in
  let header = history_table_header ~label_width ~column_width ~current_partial history in
  println header;
  gc_metric_specs
  |> List.for_each
    ~fn:(fun (label, project) ->
      let current_value = project current.gc in
      let baseline_value = project baseline.gc in
      let delta_value = current_value - baseline_value in
      let delta_cell =
        render_signed_int delta_value
        |> String.pad_left ~width:column_width ' '
        |> style_signed_int_delta delta_value
      in
      let values =
        ([ String.pad_right ~width:label_width ' ' label ]
        @ [ delta_cell; String.pad_left ~width:column_width ' ' (Int.to_string current_value) ])
        @ (
          history
          |> List.map
            ~fn:(fun (sample: History.history_sample) ->
              String.pad_left
                ~width:column_width
                ' '
                (Int.to_string (project sample.statistics.gc)))
        )
        |> String.concat " "
      in
      println values)

let print_gc_rate_history_table = fun
  ~current_partial
  ~(baseline:History.bench_statistics)
  (current: History.bench_statistics)
  (history: History.history_sample list) ->
  let label_width = 10 in
  let column_width = 12 in
  let header = history_table_header ~label_width ~column_width ~current_partial history in
  println header;
  gc_metric_specs
  |> List.for_each
    ~fn:(fun (label, project) ->
      let current_value = gc_per_iteration (project current.gc) current.iterations in
      let baseline_value = gc_per_iteration (project baseline.gc) baseline.iterations in
      let delta_value =
        match (current_value, baseline_value) with
        | (Some current_value, Some baseline_value) -> float_delta current_value baseline_value
        | _ -> None
      in
      let delta_cell =
        match delta_value with
        | Some delta_value ->
            render_signed_float delta_value
            |> String.pad_left ~width:column_width ' '
            |> style_signed_float_delta delta_value
        | None -> String.pad_left ~width:column_width ' ' "n/a"
      in
      let values =
        ([ String.pad_right ~width:label_width ' ' label ]
        @ [ delta_cell; String.pad_left ~width:column_width ' ' (render_gc_rate current_value) ]) @ (
          history
          |> List.map
            ~fn:(fun (sample: History.history_sample) ->
              let value =
                gc_per_iteration (project sample.statistics.gc) sample.statistics.iterations
              in
              String.pad_left ~width:column_width ' ' (render_gc_rate value))
        )
        |> String.concat " "
      in
      println values)

let print_benchmark_history = fun ~current_partial (history: History.benchmark_history) ->
  println "  history:";
  println
    ("  baseline: median of previous " ^ Int.to_string (List.length history.history) ^ " runs");
  println
    ("  noise margin: "
    ^ print_percent
      ((noise_margin_percent ~current_cv:history.current_cv ~baseline_cv:history.baseline_cv) /. 100.0));
  println
    ("  stability: "
    ^ styled_stability_label history.stability
    ^ " (cv "
    ^ style_cv_text history.current_cv (render_cv history.current_cv)
    ^ ", previous median "
    ^ style_cv_text history.baseline_cv (render_cv history.baseline_cv)
    ^ ")");
  print_history_table
    ~current_partial
    ~baseline:history.baseline
    ~current_cv:history.current_cv
    ~baseline_cv:history.baseline_cv
    history.current
    history.history;
  println "";
  println "  gc:";
  print_gc_history_table ~current_partial ~baseline:history.baseline history.current history.history;
  println "";
  println "  gc / iter:";
  print_gc_rate_history_table
    ~current_partial
    ~baseline:history.baseline
    history.current
    history.history;
  println ""

let print_comparison_case_history = fun
  ~current_partial (history: History.comparison_case_history) ->
  println ("  history: " ^ history.description ^ " / " ^ history.name);
  println
    ("  baseline: median of previous " ^ Int.to_string (List.length history.history) ^ " runs");
  println
    ("  noise margin: "
    ^ print_percent
      ((noise_margin_percent ~current_cv:history.current_cv ~baseline_cv:history.baseline_cv) /. 100.0));
  println
    ("  stability: "
    ^ styled_stability_label history.stability
    ^ " (cv "
    ^ style_cv_text history.current_cv (render_cv history.current_cv)
    ^ ", previous median "
    ^ style_cv_text history.baseline_cv (render_cv history.baseline_cv)
    ^ ")");
  print_history_table
    ~current_partial
    ~baseline:history.baseline
    ~current_cv:history.current_cv
    ~baseline_cv:history.baseline_cv
    history.current
    history.history;
  println "";
  println "  gc:";
  print_gc_history_table ~current_partial ~baseline:history.baseline history.current history.history;
  println "";
  println "  gc / iter:";
  print_gc_rate_history_table
    ~current_partial
    ~baseline:history.baseline
    history.current
    history.history;
  println ""

let history_statistics_json = fun (stats: History.bench_statistics) ->
  Data.Json.Object [
    ("min_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.min)));
    ("max_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.max)));
    ("mean_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.mean)));
    ("median_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.median)));
    ("std_dev_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.std_dev)));
    ("iterations", Data.Json.Int stats.iterations);
    ("total_time_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.total_time)));
    (
      "gc",
      Data.Json.Object [
        ("minor_collections", Data.Json.Int stats.gc.minor_collections);
        ("major_collections", Data.Json.Int stats.gc.major_collections);
        ("compactions", Data.Json.Int stats.gc.compactions);
      ]
    );
  ]

let history_sample_json = fun (sample: History.history_sample) ->
  Data.Json.Object [
    ("run_id", Data.Json.String sample.run_id);
    ("partial", Data.Json.Bool sample.partial);
    ("statistics", history_statistics_json sample.statistics);
  ]

let benchmark_history_json = fun (history: History.benchmark_history) ->
  Data.Json.Object [
    ("index", Data.Json.Int history.index);
    ("name", Data.Json.String history.name);
    ("current", history_statistics_json history.current);
    ("baseline", history_statistics_json history.baseline);
    ("current_cv", json_of_option history.current_cv ~some:(fun value -> Data.Json.Float value));
    ("baseline_cv", json_of_option history.baseline_cv ~some:(fun value -> Data.Json.Float value));
    ("stability", Data.Json.String (
      match history.stability with
      | History.Stable -> "stable"
      | History.Noisy -> "noisy"
    ));
    ("history", Data.Json.Array (List.map history.history ~fn:history_sample_json));
  ]

let comparison_case_history_json = fun (history: History.comparison_case_history) ->
  Data.Json.Object [
    ("description", Data.Json.String history.description);
    ("name", Data.Json.String history.name);
    ("current", history_statistics_json history.current);
    ("baseline", history_statistics_json history.baseline);
    ("current_cv", json_of_option history.current_cv ~some:(fun value -> Data.Json.Float value));
    ("baseline_cv", json_of_option history.baseline_cv ~some:(fun value -> Data.Json.Float value));
    ("stability", Data.Json.String (
      match history.stability with
      | History.Stable -> "stable"
      | History.Noisy -> "noisy"
    ));
    ("history", Data.Json.Array (List.map history.history ~fn:history_sample_json));
  ]

let write_bench_history_json = fun
  ~command_started_at
  ~current_partial
  (suite: Bench_runtime.suite_binary)
  (history: History.suite_history) ->
  write_json_line
    (Data.Json.Object [
      ("type", Data.Json.String "BenchHistoryCompared");
      ("package", Data.Json.String (Package_name.to_string suite.package_name));
      ("suite", Data.Json.String suite.suite_name);
      ("current_partial", Data.Json.Bool current_partial);
      ("benchmarks", Data.Json.Array (List.map history.benchmarks ~fn:benchmark_history_json));
      (
        "comparisons",
        Data.Json.Array (List.map history.comparisons ~fn:comparison_case_history_json)
      );
      ("emitted_at_us", Data.Json.Int (event_elapsed_us ~command_started_at));
    ])

let bench_history_warning = fun message -> eprintln ("warning: " ^ message)

type human_render_state = {
  mutable active_suite: Bench_runtime.suite_binary option;
  mutable suite_header_printed: bool;
  mutable active_case: Bench_runtime.running_bench_case option;
  mutable active_case_line_open: bool;
}

let create_human_render_state = fun () ->
  {
    active_suite = None;
    suite_header_printed = false;
    active_case = None;
    active_case_line_open = false;
  }

let ensure_suite_header = fun state suite ->
  if not state.suite_header_printed then (
    print_run_label suite;
    state.active_suite <- Some suite;
    state.suite_header_printed <- true
  )

let reset_active_case_line = fun state ->
  if state.active_case_line_open then (
    println "";
    state.active_case_line_open <- false
  );
  state.active_case <- None

let reset_suite_render = fun state ->
  reset_active_case_line state;
  state.active_suite <- None;
  state.suite_header_printed <- false

let json_int_field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, Data.Json.Int value) -> Some value
  | _ -> None

let upsert_int_field = fun name value fields ->
  let filtered =
    List.filter fields ~fn:(fun (field_name, _) -> not (String.equal field_name name))
  in
  filtered @ [ (name, Data.Json.Int value); ]

let stamp_json_event = fun
  ~command_started_at ~duration_us (event: Bench_runtime.bench_event) (json: Data.Json.t) ->
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
        | Bench_runtime.SuiteHeartbeat _ -> upsert_int_field "emitted_at_us" elapsed_us fields
        | Bench_runtime.SuiteProgress _ -> fields
        | Bench_runtime.SuiteCompleted _ ->
            fields
            |> upsert_int_field "started_at_us" (Int.max 0 (elapsed_us - duration_us))
            |> upsert_int_field "completed_at_us" elapsed_us
        | Bench_runtime.Summary _ ->
            fields
            |> upsert_int_field "started_at_us" 0
            |> upsert_int_field "completed_at_us" elapsed_us
        | Bench_runtime.NoSuitesFound _ -> upsert_int_field "completed_at_us" elapsed_us fields
        | Bench_runtime.Build _ -> fields
      in
      Data.Json.Object fields
  | other -> other

let write_json_event = fun ~command_started_at ~duration_us event (json: Data.Json.t) ->
  println
    (Data.Json.to_string (stamp_json_event ~command_started_at ~duration_us event json))

let summary_duration_us = fun ~command_started_at (event: Bench_runtime.bench_event) ->
  match event with
  | Bench_runtime.Summary _ ->
      Some (
        Time.Instant.elapsed command_started_at
        |> Time.Duration.to_micros
      )
  | _ -> None

let write_bench_event = fun
  ?history_comparison ~current_partial state (event: Bench_runtime.bench_event) ->
  match event with
  | Bench_runtime.Build _ -> ()
  | Bench_runtime.NoSuitesFound { package_name } ->
      reset_suite_render state;
      print_empty_hint package_name
  | Bench_runtime.RunningSuite suite ->
      reset_suite_render state;
      state.active_suite <- Some suite
  | Bench_runtime.SuiteHeartbeat { suite; active_case; _ } ->
      if Option.is_some active_case && state.active_case_line_open then (
        ensure_suite_header state suite;
        print "."
      )
  | Bench_runtime.SuiteProgress { suite; event } ->
      Bench_runtime.suite_progress_active_case event
      |> Result.iter
        ~fn:(fun active_case ->
          active_case
          |> Option.for_each
            ~fn:(fun (active_case: Bench_runtime.running_bench_case) ->
              ensure_suite_header state suite;
              reset_active_case_line state;
              print ("[" ^ Int.to_string active_case.index ^ "] " ^ active_case.name);
              state.active_case <- Some active_case;
              state.active_case_line_open <- true))
  | Bench_runtime.SuiteCompleted {
      suite;
      stdout;
      stderr;
      results;
      comparisons;
      _;
    } ->
      let should_print_suite =
        not (results = [])
        || not (comparisons = [])
        || not (String.equal stdout "")
        || not (String.equal stderr "")
      in
      reset_active_case_line state;
      if should_print_suite then (
        ensure_suite_header state suite;
        List.for_each
          results
          ~fn:(fun result ->
            print_bench_result result;
            history_comparison
            |> Option.for_each
              ~fn:(fun (history: History.suite_history) ->
                history.benchmarks
                |> List.find
                  ~fn:(fun (benchmark_history: History.benchmark_history) ->
                    String.equal
                      benchmark_history.name
                      result.name)
                |> Option.for_each ~fn:(print_benchmark_history ~current_partial)));
        List.for_each
          comparisons
          ~fn:(fun comparison ->
            print_comparison comparison;
            history_comparison
            |> Option.for_each
              ~fn:(fun (history: History.suite_history) ->
                history.comparisons
                |> List.filter
                  ~fn:(fun (comparison_history: History.comparison_case_history) ->
                    String.equal
                      comparison_history.description
                      comparison.description)
                |> List.for_each ~fn:(print_comparison_case_history ~current_partial)));
        print_command_output Command.{ stdout; stderr; status = 0 }
      );
      reset_suite_render state
  | Bench_runtime.Summary {
      total;
      completed;
      skipped;
      failed;
    } ->
      reset_suite_render state;
      print_summary ~total ~completed ~skipped ~failed

let write_bench_error = fun err -> println ("error: " ^ Bench_runtime.bench_error_message err)

let write_bench_error_json = fun ~command_started_at err ->
  let event_json = Data.Json.Object [
    ("type", Data.Json.String "bench.error");
    ("message", Data.Json.String (Bench_runtime.bench_error_message err));
  ]
  in
  print
    (
      Data.Json.to_string
        (
          match event_json with
          | Data.Json.Object fields ->
              Data.Json.Object (upsert_int_field
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

let bench_history_context = fun
  ~(workspace:Riot_model.Workspace.t) ~profile (request: Test_selection.request) ~argv ->
  History.create_run_context
    ~workspace_root:workspace.root
    ~profile
    ~filter:request.query
    ~partial:(bench_history_partial request)
    ~argv
    ()

let bench_history_of_gc = fun (gc: Bench_runtime.bench_gc_stats): History.gc_stats -> {
  minor_collections = gc.minor_collections;
  major_collections = gc.major_collections;
  compactions = gc.compactions;
}

let bench_history_of_statistics = fun
  (stats: Bench_runtime.bench_statistics): History.bench_statistics ->
  {
    min = stats.min;
    max = stats.max;
    mean = stats.mean;
    median = stats.median;
    std_dev = stats.std_dev;
    iterations = stats.iterations;
    total_time = stats.total_time;
    gc = bench_history_of_gc stats.gc;
  }

let bench_history_of_result = fun
  (result: Bench_runtime.bench_case_result): History.bench_case_result ->
  let result_status: History.bench_case_status =
    match result.result with
    | Bench_runtime.Completed stats -> History.Completed (bench_history_of_statistics stats)
    | Bench_runtime.Failed message -> History.Failed message
    | Bench_runtime.Skipped -> History.Skipped
  in
  { index = result.index; name = result.name; result = result_status }

let bench_history_of_comparison = fun
  (comparison: Bench_runtime.bench_comparison_result): History.bench_comparison_result ->
  {
    description = comparison.description;
    fastest = comparison.fastest;
    case_results = List.map
      comparison.case_results
      ~fn:(fun (case_result: Bench_runtime.bench_comparison_case_result) -> {
        History.name = case_result.name;
        statistics = bench_history_of_statistics case_result.statistics;
      });
    speedup_ratios = comparison.speedup_ratios;
  }

let save_bench_history = fun
  context
  ~(suite:Bench_runtime.suite_binary)
  status
  started_at_us
  completed_at_us
  duration_us
  (results: Bench_runtime.bench_case_result list)
  (comparisons: Bench_runtime.bench_comparison_result list)
  (summary: Bench_runtime.bench_suite_summary) ->
  let suite_run: History.suite_run = {
    status;
    started_at_us;
    completed_at_us;
    duration_us;
    summary =
      {
        total = summary.total;
        completed = summary.completed;
        skipped = summary.skipped;
        failed = summary.failed;
      };
    benchmarks = List.map results ~fn:bench_history_of_result;
    comparisons = List.map comparisons ~fn:bench_history_of_comparison;
  }
  in
  History.save_suite_run
    context
    ~package_name:suite.package_name
    ~suite_name:suite.suite_name
    ~suite_run

let bench_history_compare = fun
  context
  ~(suite:Bench_runtime.suite_binary)
  ~limit
  status
  started_at_us
  completed_at_us
  duration_us
  (results: Bench_runtime.bench_case_result list)
  (comparisons: Bench_runtime.bench_comparison_result list)
  (summary: Bench_runtime.bench_suite_summary) ->
  if Int.(limit <= 0) then
    Ok None
  else
    let current: History.suite_run = {
      status;
      started_at_us;
      completed_at_us;
      duration_us;
      summary =
        {
          total = summary.total;
          completed = summary.completed;
          skipped = summary.skipped;
          failed = summary.failed;
        };
      benchmarks = List.map results ~fn:bench_history_of_result;
      comparisons = List.map comparisons ~fn:bench_history_of_comparison;
    }
    in
    History.compare_suite_run
      context
      ~package_name:suite.package_name
      ~suite_name:suite.suite_name
      ~current
      ~limit
    |> Result.map
      ~fn:(fun (history: History.suite_history) ->
        if List.is_empty history.benchmarks && List.is_empty history.comparisons then
          None
        else
          Some history)

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let seen_registry_updates = Collections.HashSet.create () in
  let* (matches, trailing) = reparsed_matches matches in
  let extra_args = trailing @ bench_override_args matches in
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
  let compare_limit = compare_limit_of_matches matches in
  let profile = profile_of_matches matches in
  let* package_filters = package_filters in
  let* compare_limit = compare_limit in
  let* request =
    Test_selection.parse_request
      ~filter:pattern
      ~package_filters
      ~size_filter:Test_selection.All
      ~flaky_only:false
    |> Result.map_err ~fn:(fun error -> Failure error)
  in
  let extra_args = Test_selection.extra_args request extra_args in
  let command_started_at = Time.Instant.now () in
  if output_mode = Build.Json then
    Build.reset_json_clock ~started_at:command_started_at;
  if list_mode then
    let listed_suite_count = ref 0 in
    let listed_benchmark_count = ref 0 in
    let failed_suite_count = ref 0 in
    let on_suite (suite: Bench_runtime.listed_bench_suite) =
      if not (List.is_empty suite.benchmarks) then (
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
    match Bench_runtime.list_benchmarks
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
      } with
    | Ok suites ->
        let suites =
          List.filter
            suites
            ~fn:(fun (suite: Bench_runtime.listed_bench_suite) ->
              not
                (List.is_empty suite.benchmarks))
        in
        (
          match output_mode with
          | Build.Json ->
              write_bench_list_completed_json
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
    let history_partial = bench_history_partial request in
    let record_history = ArgParser.get_flag matches "record" in
    let history_context =
      if record_history || Option.is_some compare_limit then
        Some (bench_history_context ~workspace ~profile request ~argv:Env.args)
      else
        None
    in
    let human_render_state = create_human_render_state () in
    let on_event (event: Bench_runtime.bench_event) =
      match event with
      | Bench_runtime.Build build_event ->
          Build.write_build_event ~mode:output_mode ~profile ~seen_registry_updates build_event
      | Bench_runtime.SuiteCompleted {
          suite;
          status;
          started_at_us;
          completed_at_us;
          duration_us;
          results;
          comparisons;
          summary;
          _;
        } ->
          let history_comparison =
            match (compare_limit, history_context) with
            | (Some compare_limit, Some history_context) ->
                Some (bench_history_compare
                  history_context
                  ~suite
                  ~limit:compare_limit
                  status
                  started_at_us
                  completed_at_us
                  duration_us
                  results
                  comparisons
                  summary)
            | _ -> None
          in
          let history_comparison =
            match history_comparison with
            | None -> None
            | Some (Ok history) -> history
            | Some (Error error) ->
                bench_history_warning
                  ("failed to compare benchmark history for "
                  ^ Package_name.to_string suite.package_name
                  ^ "/"
                  ^ suite.suite_name
                  ^ ": "
                  ^ error);
                None
          in
          if record_history then
            history_context
            |> Option.for_each
              ~fn:(fun history_context ->
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
                      ^ error)));
          (
            match output_mode with
            | Build.Json ->
                Bench_runtime.bench_event_to_json event
                |> Option.for_each
                  ~fn:(fun json ->
                    write_json_event
                      ~command_started_at
                      ~duration_us:(summary_duration_us ~command_started_at event)
                      event
                      json);
                history_comparison
                |> Option.for_each
                  ~fn:(fun history ->
                    write_bench_history_json
                      ~command_started_at
                      ~current_partial:history_partial
                      suite
                      history)
            | Build.Human ->
                write_bench_event
                  ?history_comparison
                  ~current_partial:history_partial
                  human_render_state
                  event
          )
      | _ -> (
          match output_mode with
          | Build.Json ->
              Bench_runtime.bench_event_to_json event
              |> Option.for_each
                ~fn:(fun json ->
                  write_json_event
                    ~command_started_at
                    ~duration_us:(summary_duration_us ~command_started_at event)
                    event
                    json)
          | Build.Human ->
              write_bench_event ~current_partial:history_partial human_render_state event
        )
    in
    match Bench_runtime.bench
      ~on_event
      {
        workspace;
        package_filters = request.package_filters;
        suite_filter = request.suite_filter;
        profile;
        extra_args;
      } with
    | Ok () -> Ok ()
    | Error err ->
        (
          match output_mode with
          | Build.Json -> write_bench_error_json ~command_started_at err
          | Build.Human -> write_bench_error err
        );
        Error (Failure (Bench_runtime.bench_error_message err))
