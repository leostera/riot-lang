open Std
open Std.Result.Syntax
open Riot_model

type suite_binary = Test_runtime.suite_binary = {
  package_name: Package_name.t;
  suite_name: string;
}

type bench_request = {
  workspace: Workspace.t;
  package_filters: Package_name.t list;
  suite_filter: string option;
  profile: string;
  extra_args: string list;
}

type bench_gc_stats = { minor_collections: int; major_collections: int; compactions: int }

type bench_statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
  gc: bench_gc_stats;
}

type bench_case_status =
  | Completed of bench_statistics
  | Failed of string
  | Skipped

type bench_case_result = {
  index: int;
  name: string;
  result: bench_case_status;
}

type listed_bench_item_kind =
  | Benchmark
  | Comparison

type listed_bench_item = {
  index: int;
  name: string;
  kind: listed_bench_item_kind;
  iterations: int;
  warmup: int;
  skip: bool;
  cases: string list;
}

type listed_bench_suite = {
  suite: suite_binary;
  source_path: Path.t option;
  benchmarks: listed_bench_item list;
}

type bench_comparison_case_result = {
  name: string;
  statistics: bench_statistics;
}

type bench_comparison_result = {
  description: string;
  case_results: bench_comparison_case_result list;
  fastest: string;
  speedup_ratios: (string * float) list;
}

type bench_suite_summary = { total: int; completed: int; skipped: int; failed: int }

type running_bench_case = { index: int; name: string; iterations: int; warmup: int }

type bench_event =
  | Build of Riot_build.Event.t
  | NoSuitesFound of {
      package_name: Package_name.t option;
    }
  | RunningSuite of suite_binary
  | SuiteHeartbeat of {
      suite: suite_binary;
      binary_path: Path.t;
      elapsed_us: int;
      active_case: running_bench_case option;
    }
  | SuiteProgress of {
      suite: suite_binary;
      event: Data.Json.t;
    }
  | SuiteCompleted of {
      suite: suite_binary;
      status: int;
      stdout: string;
      stderr: string;
      started_at_us: int option;
      completed_at_us: int option;
      duration_us: int option;
      results: bench_case_result list;
      comparisons: bench_comparison_result list;
      summary: bench_suite_summary;
    }
  | Summary of { total: int; completed: int; skipped: int; failed: int }

type bench_error =
  | BuildFailed of Riot_build.error
  | SuiteArtifactNotFound of {
      suite: suite_binary;
      reason: string;
    }
  | SuiteExecutionError of {
      suite: suite_binary;
      reason: string;
    }
  | SuitesFailed of int

type Message.t +=
  | ListedBenchmarksReady of (suite_binary * (listed_bench_suite, bench_error) result)

let no_event: bench_event -> unit = fun _ -> ()

let no_listed_suite: listed_bench_suite -> unit = fun _ -> ()

let no_list_error: suite_binary -> bench_error -> unit = fun _ _ -> ()

let upsert_json_field = fun name value fields ->
  let filtered =
    List.filter fields ~fn:(fun (field_name, _) -> not (String.equal field_name name))
  in
  filtered @ [ (name, value); ]

let json_event_type = fun json ->
  match Data.Json.get_field "type" json with
  | Some (Data.Json.String value) -> Some value
  | _ -> None

let suite_progress_event_of_line = fun line ->
  let trimmed = String.trim line in
  if String.equal trimmed "" then
    None
  else
    match Data.Json.from_string trimmed with
    | Ok (Data.Json.Object _ as json) -> (
        match json_event_type json with
        | Some _ -> Some json
        | None -> None
      )
    | Ok _
    | Error _ -> None

let strip_progress_json_lines = fun lines ->
  lines
  |> List.filter ~fn:(fun line -> Option.is_none (suite_progress_event_of_line line))

let bench_progress_json = fun (suite: suite_binary) (json: Data.Json.t) ->
  match json with
  | Data.Json.Object fields ->
      Data.Json.Object (
        fields
        |> upsert_json_field
          "package"
          (Data.Json.String (Package_name.to_string suite.package_name))
        |> upsert_json_field "suite" (Data.Json.String suite.suite_name)
      )
  | other -> other

let is_benchmark_binary_name = fun name ->
  String.ends_with ~suffix:"_bench" name || String.ends_with ~suffix:"-bench" name

let compare_suite_binary = fun left right ->
  match Package_name.compare left.package_name right.package_name with
  | Order.EQ -> String.compare left.suite_name right.suite_name
  | cmp -> cmp

let requested_packages = fun suites ->
  suites
  |> List.map ~fn:(fun (suite: suite_binary) -> suite.package_name)
  |> List.unique ~compare:Package_name.compare

let profile_of_name = fun __tmp1 ->
  match __tmp1 with
  | "release" -> Riot_model.Profile.release
  | _ -> Riot_model.Profile.debug

let matches_package_filters = fun package_filters package_name ->
  List.is_empty package_filters
  || List.exists
    (fun package_filter -> Package_name.equal package_filter package_name)
    package_filters

let selected_package_name = fun __tmp1 ->
  match __tmp1 with
  | [ package_name ] -> Some package_name
  | _ -> None

let realized_bench_packages = fun ?(package_filters = []) (workspace: Workspace.t) ->
  Workspace.realize_packages ~intent:Package.Bench workspace
  |> List.filter ~fn:Package.is_workspace_member
  |> List.filter ~fn:(fun (pkg: Package.t) -> matches_package_filters package_filters pkg.name)

let collect_suite_binaries = fun
  (workspace: Workspace.t) ?(package_filters = []) ?suite_filter () ->
  realized_bench_packages ~package_filters workspace
  |> List.flat_map
    ~fn:(fun (pkg: Package.t) ->
      List.filter_map
        pkg.binaries
        ~fn:(fun (bin: Package.binary) ->
          if is_benchmark_binary_name bin.name && (
            match suite_filter with
            | None -> true
            | Some suite_name -> String.equal bin.name suite_name
          ) then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None))
  |> List.sort ~compare:compare_suite_binary

let find_suite_source_path = fun ~(workspace:Workspace.t) (suite: suite_binary) ->
  match List.find
    (realized_bench_packages workspace)
    ~fn:(fun (pkg: Package.t) -> Package_name.equal pkg.name suite.package_name) with
  | None -> None
  | Some pkg ->
      List.find
        pkg.binaries
        ~fn:(fun (bin: Package.binary) -> String.equal bin.name suite.suite_name)
      |> Option.map ~fn:(fun (bin: Package.binary) -> Path.(pkg.path / bin.path))

let bench_error_message = fun __tmp1 ->
  match __tmp1 with
  | BuildFailed err -> Riot_build.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " benchmark suite(s) failed"

let rec json_type_name = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed json -> json_type_name json

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let get_float = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Float value -> Ok value
  | Data.Json.Int value -> Ok (Float.from_int value)
  | other -> error_expected "float" other

let get_bool = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Bool value -> Ok value
  | other -> error_expected "bool" other

let field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, value) -> Ok value
  | None -> Error ("missing field " ^ name)

let optional_int_field = fun name fields ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, value) ->
      get_int value
      |> Result.map ~fn:Option.some
  | None -> Ok None

let running_bench_case_of_json = fun json ->
  let* fields = get_object json in
  let* type_json = field "type" fields in
  let* event_type = get_string type_json in
  if not (String.equal event_type "BenchCaseStarted") then
    Ok None
  else
    let* index_json = field "index" fields in
    let* name_json = field "name" fields in
    let* iterations_json = field "iterations" fields in
    let* warmup_json = field "warmup" fields in
    let* index = get_int index_json in
    let* name = get_string name_json in
    let* iterations = get_int iterations_json in
    let* warmup = get_int warmup_json in
    Ok (
      Some {
        index;
        name;
        iterations;
        warmup;
      }
    )

let suite_progress_active_case = running_bench_case_of_json

let split_json_stdout = fun stdout ->
  let lines = String.split stdout ~by:"\n" in
  let indexed = List.enumerate lines in
  match indexed
  |> List.reverse
  |> List.find ~fn:(fun (_, line) -> not (String.equal (String.trim line) "")) with
  | None -> Error "missing JSON output"
  | Some (json_idx, json_line) ->
      let prefix =
        indexed
        |> List.filter_map
          ~fn:(fun (idx, line) ->
            if idx < json_idx then
              Some line
            else
              None)
        |> strip_progress_json_lines
        |> String.concat "\n"
      in
      Ok (prefix, json_line)

let remove_json_args = fun args ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let remove_list_args = fun args ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let duration_of_nanos_json = fun json ->
  let* nanos = get_int json in
  Ok (Time.Duration.from_nanos nanos)

let gc_stats_of_json = fun json ->
  let* fields = get_object json in
  let* minor_collections_json = field "minor_collections" fields in
  let* major_collections_json = field "major_collections" fields in
  let* compactions_json = field "compactions" fields in
  let* minor_collections = get_int minor_collections_json in
  let* major_collections = get_int major_collections_json in
  let* compactions = get_int compactions_json in
  Ok { minor_collections; major_collections; compactions }

let statistics_of_json = fun json ->
  let* fields = get_object json in
  let* min_json = field "min_nanos" fields in
  let* max_json = field "max_nanos" fields in
  let* mean_json = field "mean_nanos" fields in
  let* median_json = field "median_nanos" fields in
  let* std_dev_json = field "std_dev_nanos" fields in
  let* iterations_json = field "iterations" fields in
  let* total_time_json = field "total_time_nanos" fields in
  let* gc_json = field "gc" fields in
  let* min = duration_of_nanos_json min_json in
  let* max = duration_of_nanos_json max_json in
  let* mean = duration_of_nanos_json mean_json in
  let* median = duration_of_nanos_json median_json in
  let* std_dev = duration_of_nanos_json std_dev_json in
  let* iterations = get_int iterations_json in
  let* total_time = duration_of_nanos_json total_time_json in
  let* gc = gc_stats_of_json gc_json in
  Ok {
    min;
    max;
    mean;
    median;
    std_dev;
    iterations;
    total_time;
    gc;
  }

let bench_status_of_json = fun json ->
  let* fields = get_object json in
  let* status_json = field "status" fields in
  let* status = get_string status_json in
  match status with
  | "completed" ->
      let* statistics_json = field "statistics" fields in
      let* statistics = statistics_of_json statistics_json in
      Ok (Completed statistics)
  | "failed" ->
      let* message_json = field "message" fields in
      let* message = get_string message_json in
      Ok (Failed message)
  | "skipped" -> Ok Skipped
  | other -> Error ("unknown benchmark status " ^ other)

let bench_result_of_json = fun json ->
  let* fields = get_object json in
  let* index_json = field "index" fields in
  let* name_json = field "name" fields in
  let* index = get_int index_json in
  let* name = get_string name_json in
  let* result = bench_status_of_json json in
  Ok { index; name; result }

let speedup_ratio_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* ratio_json = field "ratio" fields in
  let* name = get_string name_json in
  let* ratio = get_float ratio_json in
  Ok (name, ratio)

let listed_bench_item_of_json = fun json ->
  let* fields = get_object json in
  let* index_json = field "index" fields in
  let* name_json = field "name" fields in
  let* kind_json = field "kind" fields in
  let* iterations_json = field "iterations" fields in
  let* warmup_json = field "warmup" fields in
  let* index = get_int index_json in
  let* name = get_string name_json in
  let* kind_name = get_string kind_json in
  let* iterations = get_int iterations_json in
  let* warmup = get_int warmup_json in
  let* kind =
    match kind_name with
    | "benchmark" -> Ok Benchmark
    | "comparison" -> Ok Comparison
    | other -> Error ("unknown benchmark list item kind " ^ other)
  in
  let skip =
    match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "skip") with
    | Some (_, value) -> get_bool value
    | None -> Ok false
  in
  let cases =
    match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name "cases") with
    | None -> Ok []
    | Some (_, value) ->
        let* values = get_array value in
        let rec loop acc = fun __tmp1 ->
          match __tmp1 with
          | [] -> Ok (List.reverse acc)
          | value :: rest ->
              let* name = get_string value in
              loop (name :: acc) rest
        in
        loop [] values
  in
  let* skip = skip in
  let* cases = cases in
  Ok {
    index;
    name;
    kind;
    iterations;
    warmup;
    skip;
    cases;
  }

let comparison_case_result_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* statistics_json = field "statistics" fields in
  let* name = get_string name_json in
  let* statistics = statistics_of_json statistics_json in
  Ok { name; statistics }

let comparison_result_of_json = fun json ->
  let* fields = get_object json in
  let* description_json = field "description" fields in
  let* fastest_json = field "fastest" fields in
  let* case_results_json = field "case_results" fields in
  let* speedup_ratios_json = field "speedup_ratios" fields in
  let* description = get_string description_json in
  let* fastest = get_string fastest_json in
  let* case_results_values = get_array case_results_json in
  let* speedup_ratio_values = get_array speedup_ratios_json in
  let rec parse_cases acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | item :: rest ->
        let* parsed = comparison_case_result_of_json item in
        parse_cases (parsed :: acc) rest
  in
  let rec parse_ratios acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | item :: rest ->
        let* parsed = speedup_ratio_of_json item in
        parse_ratios (parsed :: acc) rest
  in
  let* case_results = parse_cases [] case_results_values in
  let* speedup_ratios = parse_ratios [] speedup_ratio_values in
  Ok {
    description;
    case_results;
    fastest;
    speedup_ratios;
  }

let bench_summary_of_json = fun json ->
  let* fields = get_object json in
  let* total_json = field "total" fields in
  let* completed_json = field "completed" fields in
  let* skipped_json = field "skipped" fields in
  let* failed_json = field "failed" fields in
  let* total = get_int total_json in
  let* completed = get_int completed_json in
  let* skipped = get_int skipped_json in
  let* failed = get_int failed_json in
  Ok {
    total;
    completed;
    skipped;
    failed;
  }

let parse_bench_suite_output = fun stdout ->
  let* (prefix_stdout, json_line) = split_json_stdout stdout in
  let* json =
    Data.Json.from_string json_line
    |> Result.map_err ~fn:Data.Json.error_to_string
  in
  let* fields = get_object json in
  let* benchmarks_json = field "benchmarks" fields in
  let* comparisons_json = field "comparisons" fields in
  let* summary_json = field "summary" fields in
  let* benchmark_values = get_array benchmarks_json in
  let* comparison_values = get_array comparisons_json in
  let rec parse_results acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | item :: rest ->
        let* parsed = bench_result_of_json item in
        parse_results (parsed :: acc) rest
  in
  let rec parse_comparisons acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | item :: rest ->
        let* parsed = comparison_result_of_json item in
        parse_comparisons (parsed :: acc) rest
  in
  let* results = parse_results [] benchmark_values in
  let* comparisons = parse_comparisons [] comparison_values in
  let* summary = bench_summary_of_json summary_json in
  let* started_at_us = optional_int_field "started_at_us" fields in
  let* completed_at_us = optional_int_field "completed_at_us" fields in
  let* duration_us = optional_int_field "duration_us" fields in
  Ok (prefix_stdout, started_at_us, completed_at_us, duration_us, results, comparisons, summary)

let bench_event_to_json = fun __tmp1 ->
  match __tmp1 with
  | Build event -> Riot_build.Event.to_json event
  | NoSuitesFound { package_name } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "NoBenchSuitesFound");
          ("package_name", match package_name with
          | Some name -> Data.Json.String (Riot_model.Package_name.to_string name)
          | None -> Data.Json.Null);
        ]
      )
  | RunningSuite { package_name; suite_name } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "RunningBenchSuite");
        ("package", Data.Json.String (Riot_model.Package_name.to_string package_name));
        ("suite", Data.Json.String suite_name);
      ])
  | SuiteHeartbeat {
      suite;
      binary_path;
      elapsed_us;
      active_case;
    } ->
      let active_case_fields =
        match active_case with
        | Some case ->
            [
              ("index", Data.Json.Int case.index);
              ("name", Data.Json.String case.name);
              ("iterations", Data.Json.Int case.iterations);
              ("warmup", Data.Json.Int case.warmup);
            ]
        | None -> []
      in
      Some (Data.Json.Object (([
        ("type", Data.Json.String "BenchCaseHeartbeat");
        ("binary_path", Data.Json.String (Path.to_string binary_path));
        ("elapsed_us", Data.Json.Int elapsed_us);
      ]
      @ active_case_fields)
      @ [
        ("package", Data.Json.String (Riot_model.Package_name.to_string suite.package_name));
        ("suite", Data.Json.String suite.suite_name);
      ]))
  | SuiteProgress { suite; event } -> Some (bench_progress_json suite event)
  | SuiteCompleted {
      suite;
      status;
      stdout;
      stderr;
      started_at_us;
      completed_at_us;
      duration_us;
      results;
      comparisons;
      summary;
    } ->
      let statistics_to_json (stats: bench_statistics) =
        Data.Json.Object [
          ("min_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.min)));
          ("max_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.max)));
          ("mean_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.mean)));
          ("median_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.median)));
          ("std_dev_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.std_dev)));
          ("iterations", Data.Json.Int stats.iterations);
          (
            "total_time_nanos",
            Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.total_time))
          );
          (
            "gc",
            Data.Json.Object [
              ("minor_collections", Data.Json.Int stats.gc.minor_collections);
              ("major_collections", Data.Json.Int stats.gc.major_collections);
              ("compactions", Data.Json.Int stats.gc.compactions);
            ]
          );
        ]
      in
      let result_to_json (result: bench_case_result) =
        let base_fields = [
          ("index", Data.Json.Int result.index);
          ("name", Data.Json.String result.name);
        ]
        in
        match result.result with
        | Completed stats ->
            Data.Json.Object (base_fields
            @ [
              ("status", Data.Json.String "completed");
              ("statistics", statistics_to_json stats);
            ])
        | Failed message ->
            Data.Json.Object (base_fields
            @ [ ("status", Data.Json.String "failed"); ("message", Data.Json.String message); ])
        | Skipped -> Data.Json.Object (base_fields @ [ ("status", Data.Json.String "skipped"); ])
      in
      let comparison_to_json (comparison: bench_comparison_result) =
        Data.Json.Object [
          ("description", Data.Json.String comparison.description);
          ("fastest", Data.Json.String comparison.fastest);
          (
            "case_results",
            Data.Json.Array (List.map
              comparison.case_results
              ~fn:(fun (case_result: bench_comparison_case_result) ->
                Data.Json.Object [
                  ("name", Data.Json.String case_result.name);
                  ("statistics", statistics_to_json case_result.statistics);
                ]))
          );
          (
            "speedup_ratios",
            Data.Json.Array (List.map
              comparison.speedup_ratios
              ~fn:(fun (name, ratio) ->
                Data.Json.Object [
                  ("name", Data.Json.String name);
                  ("ratio", Data.Json.Float ratio);
                ]))
          );
        ]
      in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "BenchSuiteCompleted");
          ("package", Data.Json.String (Riot_model.Package_name.to_string suite.package_name));
          ("suite", Data.Json.String suite.suite_name);
          ("status", Data.Json.Int status);
          ("stdout", Data.Json.String stdout);
          ("stderr", Data.Json.String stderr);
          ("started_at_us", match started_at_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Null);
          ("completed_at_us", match completed_at_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Null);
          ("duration_us", match duration_us with
          | Some value -> Data.Json.Int value
          | None -> Data.Json.Null);
          ("benchmarks", Data.Json.Array (List.map results ~fn:result_to_json));
          ("comparisons", Data.Json.Array (List.map comparisons ~fn:comparison_to_json));
          (
            "summary",
            Data.Json.Object [
              ("total", Data.Json.Int summary.total);
              ("completed", Data.Json.Int summary.completed);
              ("skipped", Data.Json.Int summary.skipped);
              ("failed", Data.Json.Int summary.failed);
            ]
          );
        ]
      )
  | Summary {
      total;
      completed;
      skipped;
      failed;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BenchSummary");
        ("total", Data.Json.Int total);
        ("completed", Data.Json.Int completed);
        ("skipped", Data.Json.Int skipped);
        ("failed", Data.Json.Int failed);
      ])

let ensure_executable_binary_path = fun ~kind path ->
  match Fs.metadata path with
  | Error err -> Error ("failed to read " ^ kind ^ " metadata: " ^ IO.error_message err)
  | Ok metadata ->
      let mode = Fs.Metadata.mode metadata in
      if mode land 0o111 != 0 then
        Ok path
      else
        Fs.set_permissions path (Fs.Permissions.from_mode (mode lor 0o111))
        |> Result.map ~fn:(fun () -> path)
        |> Result.map_err
          ~fn:(fun err -> "failed to mark " ^ kind ^ " executable: " ^ IO.error_message err)

let materialized_suite_binary_path = fun
  ~(workspace:Workspace.t) ~profile ~(suite:suite_binary) ->
  let out_dir =
    Riot_model.Riot_dirs.out_dir_in_workspace
      ~workspace
      ~profile
      ~target:(Riot_model.Riot_dirs.host_target ())
  in
  Path.(out_dir / Path.v (Package_name.to_string suite.package_name) / Path.v suite.suite_name)

let find_suite_binary_path_in_output = fun
  ~(workspace:Workspace.t)
  ~profile
  ~(store:Riot_store.Store.t)
  ~(suite:suite_binary)
  (output: Riot_build.Build_result.t) ->
  let fallback_path = materialized_suite_binary_path ~workspace ~profile ~suite in
  let ensure_materialized_fallback () =
    match Fs.exists fallback_path with
    | Ok true ->
        ensure_executable_binary_path ~kind:"benchmark binary" fallback_path
        |> Result.map_err ~fn:(fun reason -> SuiteArtifactNotFound { suite; reason })
    | Ok false
    | Error _ ->
        Error (SuiteArtifactNotFound {
          suite;
          reason = "suite '" ^ suite.suite_name ^ "' was not produced by build output";
        })
  in
  match Riot_build.Build_result.find_package output suite.package_name
  |> Option.and_then
    ~fn:(fun package_output -> Riot_build.Build_result.find_export package_output suite.suite_name) with
  | None -> ensure_materialized_fallback ()
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path ->
          ensure_executable_binary_path ~kind:"benchmark binary" path
          |> Result.map_err ~fn:(fun reason -> SuiteArtifactNotFound { suite; reason })
      | None -> ensure_materialized_fallback ()
    )

let run_suite_args = fun extra_args ->
  ("run-benchmarks" :: remove_json_args extra_args) @ [ "--json" ]

let run_suite = fun ~on_event ~suite ~extra_args binary_path ->
  let args = run_suite_args extra_args in
  let cmd =
    Command.make
      (Path.to_string binary_path)
      ~env:[ ("RIOT_PACKAGE_NAME", Package_name.to_string suite.package_name); ]
      ~args
  in
  let active_case = ref None in
  match Command.output
    cmd
    ~on_idle:(fun elapsed ->
      on_event
        (
          SuiteHeartbeat {
            suite;
            binary_path;
            elapsed_us = Time.Duration.to_micros elapsed;
            active_case = !active_case;
          }
        ))
    ~on_stdout_line:(fun line ->
      suite_progress_event_of_line line
      |> Option.for_each
        ~fn:(fun event ->
          running_bench_case_of_json event
          |> Result.iter
            ~fn:(fun current_case ->
              current_case
              |> Option.for_each ~fn:(fun current_case -> active_case := Some current_case));
          on_event (SuiteProgress { suite; event }))) with
  | Error (Command.SystemError reason) -> Error (SuiteExecutionError { suite; reason })
  | Ok output -> Ok output

let list_suite_binary_capture = fun ~suite ~extra_args binary_path ->
  let extra_args = remove_list_args extra_args @ [ "--json" ] in
  let cmd =
    Command.make
      (Path.to_string binary_path)
      ~env:[ ("RIOT_PACKAGE_NAME", Package_name.to_string suite.package_name); ]
      ~args:("list-benchmarks" :: extra_args)
  in
  Command.output cmd

let parse_listed_benchmarks_output = fun stdout ->
  let* (_prefix_stdout, json_line) = split_json_stdout stdout in
  let* json =
    Data.Json.from_string json_line
    |> Result.map_err ~fn:Data.Json.error_to_string
  in
  let* fields = get_object json in
  let* benchmarks_json = field "benchmarks" fields in
  let* benchmarks = get_array benchmarks_json in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | json :: rest ->
        let* listed = listed_bench_item_of_json json in
        loop (listed :: acc) rest
  in
  loop [] benchmarks

let list_suite = fun ~(workspace:Workspace.t) ~suite ~extra_args binary_path ->
  match list_suite_binary_capture ~suite ~extra_args binary_path with
  | Error (Command.SystemError reason) -> Error (SuiteExecutionError { suite; reason })
  | Ok output -> (
      match parse_listed_benchmarks_output output.stdout with
      | Error reason ->
          Error (SuiteExecutionError {
            suite;
            reason = "failed to parse benchmark results from suite '"
            ^ suite.suite_name
            ^ "': "
            ^ reason;
          })
      | Ok benchmarks ->
          Ok { suite; source_path = find_suite_source_path ~workspace suite; benchmarks }
    )

let build_output = fun ~(workspace:Workspace.t) ~packages ~profile ?on_event () ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets:Target.Host
    ~scope:Riot_build.Request.Dev
    ~profile:(profile_of_name profile)
    ()
  |> Riot_build.build ?on_event

let store_for_request = fun (request: bench_request) ->
  Riot_store.Store.create_for_lane
    ~workspace:request.workspace
    ~profile:request.profile
    ~target:(Riot_dirs.host_target ())

let resolve_suite_binaries = fun ~(workspace:Workspace.t) ~profile ~store ~suites output ->
  let rec loop resolved missing = fun __tmp1 ->
    match __tmp1 with
    | [] -> (List.reverse resolved, List.reverse missing)
    | suite :: rest -> (
        match find_suite_binary_path_in_output ~workspace ~profile ~store ~suite output with
        | Ok binary_path -> loop ((suite, binary_path) :: resolved) missing rest
        | Error err -> loop resolved ((suite, err) :: missing) rest
      )
  in
  loop [] [] suites

let list_benchmarks = fun
  ?(on_suite = no_listed_suite) ?(on_suite_error = no_list_error) (request: bench_request) ->
  let suites =
    collect_suite_binaries
      request.workspace
      ~package_filters:request.package_filters
      ?suite_filter:request.suite_filter
      ()
  in
  if suites = [] then
    Ok []
  else
    match build_output
      ~workspace:request.workspace
      ~packages:(requested_packages suites)
      ~profile:request.profile
      () with
    | Error err -> Error (BuildFailed err)
    | Ok output ->
        let store = store_for_request request in
        let (suite_binaries, missing_suites) =
          resolve_suite_binaries
            ~workspace:request.workspace
            ~profile:request.profile
            ~store
            ~suites
            output
        in
        List.for_each missing_suites ~fn:(fun (suite, err) -> on_suite_error suite err);
        if suite_binaries = [] then
          Ok []
        else
          let concurrency = Int.max 1 (Int.min 8 Thread.available_parallelism) in
          let parent = self () in
          let rec spawn_initial active remaining =
            if active >= concurrency then
              (active, remaining)
            else
              match remaining with
              | [] -> (active, [])
              | (suite, binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite
                            ~extra_args:request.extra_args
                            binary_path with
                          | exn ->
                              Error (SuiteExecutionError {
                                suite;
                                reason = Exception.to_string exn;
                              })
                        in
                        send parent (ListedBenchmarksReady (suite, result));
                        Ok ())
                  in
                  spawn_initial (active + 1) rest
          in
          let rec collect active remaining acc =
            if active <= 0 then
              Ok (List.reverse acc)
            else
              let (suite, result) =
                receive
                  ~selector:(fun (msg: Message.t) ->
                    match msg with
                    | ListedBenchmarksReady payload -> Select payload
                    | _ -> Skip)
                  ()
              in
              let acc =
                match result with
                | Ok listed ->
                    on_suite listed;
                    (suite, listed) :: acc
                | Error err ->
                    on_suite_error suite err;
                    acc
              in
              match remaining with
              | [] -> collect (active - 1) [] acc
              | (next_suite, next_binary_path) :: rest ->
                  let _worker =
                    spawn
                      (fun () ->
                        let result =
                          try list_suite
                            ~workspace:request.workspace
                            ~suite:next_suite
                            ~extra_args:request.extra_args
                            next_binary_path with
                          | exn ->
                              Error (SuiteExecutionError {
                                suite = next_suite;
                                reason = Exception.to_string exn;
                              })
                        in
                        send parent (ListedBenchmarksReady (next_suite, result));
                        Ok ())
                  in
                  collect active rest acc
          in
          let (initial_active, remaining) = spawn_initial 0 suite_binaries in
          collect initial_active remaining []
          |> Result.map
            ~fn:(fun collected ->
              collected
              |> List.sort ~compare:(fun (left, _) (right, _) -> compare_suite_binary left right)
              |> List.map ~fn:(fun (_, value) -> value))

let bench = fun ?(on_event = no_event) (request: bench_request) ->
  let suites =
    collect_suite_binaries
      request.workspace
      ~package_filters:request.package_filters
      ?suite_filter:request.suite_filter
      ()
  in
  if suites = [] then (
    on_event (NoSuitesFound { package_name = selected_package_name request.package_filters });
    Ok ()
  ) else
    match build_output
      ~workspace:request.workspace
      ~packages:(requested_packages suites)
      ~profile:request.profile
      ~on_event:(fun event -> on_event (Build event))
      () with
    | Error err -> Error (BuildFailed err)
    | Ok output ->
        let store = store_for_request request in
        let total = ref 0 in
        let completed = ref 0 in
        let skipped = ref 0 in
        let failed = ref 0 in
        let rec loop = fun __tmp1 ->
          match __tmp1 with
          | [] ->
              on_event
                (
                  Summary {
                    total = !total;
                    completed = !completed;
                    skipped = !skipped;
                    failed = !failed;
                  }
                );
              if !failed > 0 then
                Error (SuitesFailed !failed)
              else
                Ok ()
          | suite :: rest -> (
              match find_suite_binary_path_in_output
                ~workspace:request.workspace
                ~profile:request.profile
                ~store
                ~suite
                output with
              | Error _ as err -> err
              | Ok binary_path ->
                  on_event (RunningSuite suite);
                  match run_suite ~on_event ~suite ~extra_args:request.extra_args binary_path with
                  | Error err -> Error err
                  | Ok output -> (
                      match parse_bench_suite_output output.stdout with
                      | Error reason ->
                          Error (SuiteExecutionError {
                            suite;
                            reason = "failed to parse benchmark results from suite '"
                            ^ suite.suite_name
                            ^ "': "
                            ^ reason;
                          })
                      | Ok (
                             stdout,
                             started_at_us,
                             completed_at_us,
                             duration_us,
                             results,
                             comparisons,
                             summary
                           ) ->
                          total := !total + summary.total;
                          completed := !completed + summary.completed;
                          skipped := !skipped + summary.skipped;
                          failed := !failed + summary.failed;
                          on_event
                            (
                              SuiteCompleted {
                                suite;
                                status = output.status;
                                stdout;
                                stderr = output.stderr;
                                started_at_us;
                                completed_at_us;
                                duration_us;
                                results;
                                comparisons;
                                summary;
                              }
                            );
                          loop rest
                    )
            )
        in
        loop suites
