open Std

type suite_binary = Test_runtime.suite_binary = {
  package_name: string;
  suite_name: string;
}

type bench_request = {
  workspace: Riot_model.Workspace.t;
  package_filter: string option;
  profile: string;
  extra_args: string list;
}

type bench_statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
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

type bench_suite_summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}

type bench_event =
  | Build of Build_runtime.build_event
  | NoSuitesFound of { package_name: string option }
  | RunningSuite of suite_binary
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
      summary: bench_suite_summary
    }
  | Summary of { total: int; completed: int; skipped: int; failed: int }

type bench_error =
  | BuildFailed of Build_runtime.build_error
  | ClientError of Client.error
  | SuiteArtifactNotFound of { suite: suite_binary; reason: string }
  | SuiteExecutionError of { suite: suite_binary; reason: string }
  | SuitesFailed of int

let no_event: bench_event -> unit = fun _ -> ()

let is_benchmark_binary_name = fun name ->
  String.ends_with ~suffix:"_bench" name || String.ends_with ~suffix:"-bench" name

let compare_suite_binary = fun left right ->
  String.compare
    (left.package_name ^ ":" ^ left.suite_name)
    (right.package_name ^ ":" ^ right.suite_name)

let requested_packages = fun suites ->
  suites |> List.map (fun (suite: suite_binary) -> suite.package_name) |> List.sort_uniq String.compare

let collect_suite_binaries = fun (workspace: Riot_model.Workspace.t) ?package_filter () ->
  workspace.packages |> List.filter Riot_model.Package.is_workspace_member |> List.filter
    (fun (pkg: Riot_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal pkg.name package_name) |> List.concat_map
    (fun (pkg: Riot_model.Package.t) ->
      List.filter_map
        (fun (bin: Riot_model.Package.binary) ->
          if is_benchmark_binary_name bin.name then
            Some { package_name = pkg.name; suite_name = bin.name }
          else
            None)
        pkg.binaries) |> List.sort compare_suite_binary

let bench_error_message = function
  | BuildFailed err -> Build_runtime.error_message err
  | ClientError err -> Client.error_message err
  | SuiteArtifactNotFound { reason; _ } -> reason
  | SuiteExecutionError { reason; _ } -> reason
  | SuitesFailed count -> Int.to_string count ^ " benchmark suite(s) failed"

let rec json_type_name = function
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

let get_object = function
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = function
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = function
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = function
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let get_float = function
  | Data.Json.Float value -> Ok value
  | Data.Json.Int value -> Ok (float_of_int value)
  | other -> error_expected "float" other

let field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let optional_int_field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> get_int value |> Result.map Option.some
  | None -> Ok None

let split_json_stdout = fun stdout ->
  let lines = String.split_on_char '\n' stdout in
  let indexed =
    List.mapi (fun idx line -> (idx, line)) lines
  in
  match indexed
  |> List.rev
  |> List.find_opt (fun (_, line) -> not (String.equal (String.trim line) "")) with
  | None -> Error "missing JSON output"
  | Some (json_idx, json_line) ->
      let prefix =
        indexed
        |> List.filter_map
          (fun (idx, line) ->
            if idx < json_idx then
              Some line
            else
              None)
        |> String.concat "\n"
      in
      Ok (prefix, json_line)

let remove_json_args = fun args ->
  let rec loop acc = function
    | [] -> List.rev acc
    | "--json" :: rest -> loop acc rest
    | "--format" :: _value :: rest -> loop acc rest
    | arg :: rest when String.starts_with ~prefix:"--format=" arg -> loop acc rest
    | arg :: rest -> loop (arg :: acc) rest
  in
  loop [] args

let duration_of_nanos_json = fun json ->
  let* nanos = get_int json in
  Ok (Time.Duration.from_nanos nanos)

let statistics_of_json = fun json ->
  let* fields = get_object json in
  let* min_json = field "min_nanos" fields in
  let* max_json = field "max_nanos" fields in
  let* mean_json = field "mean_nanos" fields in
  let* median_json = field "median_nanos" fields in
  let* std_dev_json = field "std_dev_nanos" fields in
  let* iterations_json = field "iterations" fields in
  let* total_time_json = field "total_time_nanos" fields in
  let* min = duration_of_nanos_json min_json in
  let* max = duration_of_nanos_json max_json in
  let* mean = duration_of_nanos_json mean_json in
  let* median = duration_of_nanos_json median_json in
  let* std_dev = duration_of_nanos_json std_dev_json in
  let* iterations = get_int iterations_json in
  let* total_time = duration_of_nanos_json total_time_json in
  Ok {
    min;
    max;
    mean;
    median;
    std_dev;
    iterations;
    total_time;
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
  | "skipped" ->
      Ok Skipped
  | other ->
      Error ("unknown benchmark status " ^ other)

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
  let rec parse_cases acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        let* parsed = comparison_case_result_of_json item in
        parse_cases (parsed :: acc) rest
  in
  let rec parse_ratios acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        let* parsed = speedup_ratio_of_json item in
        parse_ratios (parsed :: acc) rest
  in
  let* case_results = parse_cases [] case_results_values in
  let* speedup_ratios = parse_ratios [] speedup_ratio_values in
  Ok { description; case_results; fastest; speedup_ratios }

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
  Ok { total; completed; skipped; failed }

let parse_bench_suite_output = fun stdout ->
  let* (prefix_stdout, json_line) = split_json_stdout stdout in
  let* json = Data.Json.of_string json_line |> Result.map_error Data.Json.error_to_string in
  let* fields = get_object json in
  let* benchmarks_json = field "benchmarks" fields in
  let* comparisons_json = field "comparisons" fields in
  let* summary_json = field "summary" fields in
  let* benchmark_values = get_array benchmarks_json in
  let* comparison_values = get_array comparisons_json in
  let rec parse_results acc = function
    | [] -> Ok (List.rev acc)
    | item :: rest ->
        let* parsed = bench_result_of_json item in
        parse_results (parsed :: acc) rest
  in
  let rec parse_comparisons acc = function
    | [] -> Ok (List.rev acc)
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

let bench_event_to_json = function
  | Build event ->
      Event.to_json event
  | NoSuitesFound { package_name } ->
      Some (
        Data.Json.Object [ ("type", Data.Json.String "NoBenchSuitesFound"); (
            "package_name",
            match package_name with
            | Some name -> Data.Json.String name
            | None -> Data.Json.Null
          ); ]
      )
  | RunningSuite { package_name; suite_name } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "RunningBenchSuite");
        ("package", Data.Json.String package_name);
        ("suite", Data.Json.String suite_name);
      ])
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
    summary
  } ->
      let statistics_to_json (stats: bench_statistics) = Data.Json.Object [
        ("min_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.min)));
        ("max_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.max)));
        ("mean_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.mean)));
        ("median_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.median)));
        ("std_dev_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.std_dev)));
        ("iterations", Data.Json.Int stats.iterations);
        ("total_time_nanos", Data.Json.Int (Int64.to_int (Time.Duration.to_nanos stats.total_time)));
      ] in
      let result_to_json (result: bench_case_result) =
        let base_fields = [
          ("index", Data.Json.Int result.index);
          ("name", Data.Json.String result.name);
        ] in
        match result.result with
        | Completed stats -> Data.Json.Object (base_fields
        @ [ ("status", Data.Json.String "completed"); ("statistics", statistics_to_json stats); ])
        | Failed message -> Data.Json.Object (base_fields
        @ [ ("status", Data.Json.String "failed"); ("message", Data.Json.String message); ])
        | Skipped -> Data.Json.Object (base_fields @ [ ("status", Data.Json.String "skipped") ])
      in
      let comparison_to_json (comparison: bench_comparison_result) = Data.Json.Object [
        ("description", Data.Json.String comparison.description);
        ("fastest", Data.Json.String comparison.fastest);
        (
          "case_results",
          Data.Json.Array (List.map
            (fun (case_result: bench_comparison_case_result) ->
              Data.Json.Object [
                ("name", Data.Json.String case_result.name);
                ("statistics", statistics_to_json case_result.statistics);
              ])
            comparison.case_results)
        );
        (
          "speedup_ratios",
          Data.Json.Array (List.map
            (fun (name, ratio) ->
              Data.Json.Object [ ("name", Data.Json.String name); ("ratio", Data.Json.Float ratio); ])
            comparison.speedup_ratios)
        );
      ] in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "BenchSuiteCompleted");
          ("package", Data.Json.String suite.package_name);
          ("suite", Data.Json.String suite.suite_name);
          ("status", Data.Json.Int status);
          ("stdout", Data.Json.String stdout);
          ("stderr", Data.Json.String stderr);
          (
            "started_at_us",
            match started_at_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Null
          );
          (
            "completed_at_us",
            match completed_at_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Null
          );
          (
            "duration_us",
            match duration_us with
            | Some value -> Data.Json.Int value
            | None -> Data.Json.Null
          );
          ("benchmarks", Data.Json.Array (List.map result_to_json results));
          ("comparisons", Data.Json.Array (List.map comparison_to_json comparisons));
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
  | Summary { total; completed; skipped; failed } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BenchSummary");
        ("total", Data.Json.Int total);
        ("completed", Data.Json.Int completed);
        ("skipped", Data.Json.Int skipped);
        ("failed", Data.Json.Int failed);
      ])

let find_suite_binary_path = fun ~(store:Riot_store.Store.t) ~(suite:suite_binary) results ->
  let find_suite_export (result: Riot_executor.Package_builder.build_result) =
    if String.equal result.package.name suite.package_name then
      match result.status with
      | Riot_executor.Package_builder.Built artifact
      | Riot_executor.Package_builder.Cached artifact ->
          List.find_opt
            (fun (entry: Riot_store.Manifest.export_entry) ->
              String.equal entry.name suite.suite_name)
            artifact.exports
      | Riot_executor.Package_builder.Skipped _
      | Riot_executor.Package_builder.Failed _ -> None
    else
      None
  in
  match List.find_map find_suite_export results with
  | None -> Error (SuiteArtifactNotFound {
    suite;
    reason = "suite '" ^ suite.suite_name ^ "' was not produced by build results"
  })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path -> Ok (Path.to_string path)
      | None -> Error (SuiteArtifactNotFound {
        suite;
        reason = "suite '" ^ suite.suite_name ^ "' resolved to an invalid absolute export path"
      })
    )

let run_suite_binary_capture = fun ~extra_args binary_path ->
  let extra_args = remove_json_args extra_args @ [ "--json" ] in
  let cmd = Command.make binary_path ~args:(("run-benchmarks" :: extra_args)) in
  Command.output cmd

let bench = fun ?(on_event = no_event) (request: bench_request) ->
  let suites = collect_suite_binaries request.workspace ?package_filter:request.package_filter () in
  if suites = [] then
    (
      on_event (NoSuitesFound { package_name = request.package_filter });
      Ok ()
    )
  else
    match
      Build_runtime.build ~record_cache_generation:false ~on_event:(fun event ->
        on_event (Build event))
        {
          workspace = request.workspace;
          packages = requested_packages suites;
          targets = Build_runtime.Host;
          scope = Build_runtime.Dev;
          profile = request.profile;
        }
    with
    | Error err -> Error (BuildFailed err)
    | Ok results ->
        let store = Riot_store.Store.create_for_lane
          ~workspace:request.workspace
          ~profile:request.profile
          ~target:(Riot_model.Riot_dirs.host_target ()) in
        let total = ref 0 in
        let completed = ref 0 in
        let skipped = ref 0 in
        let failed = ref 0 in
        let rec loop = function
          | [] ->
              on_event
                (Summary {
                  total = !total;
                  completed = !completed;
                  skipped = !skipped;
                  failed = !failed
                });
              if !failed > 0 then
                Error (SuitesFailed !failed)
              else
                Ok ()
          | suite :: rest -> (
              match find_suite_binary_path ~store ~suite results with
              | Error _ as err -> err
              | Ok binary_path ->
                  on_event (RunningSuite suite);
                  match run_suite_binary_capture ~extra_args:request.extra_args binary_path with
                  | Error (Command.SystemError reason) -> Error (SuiteExecutionError {
                    suite;
                    reason
                  })
                  | Ok output -> (
                      match parse_bench_suite_output output.stdout with
                      | Error reason -> Error (SuiteExecutionError {
                        suite;
                        reason = "failed to parse benchmark results from suite '"
                        ^ suite.suite_name
                        ^ "': "
                        ^ reason
                      })
                      | Ok (stdout, started_at_us, completed_at_us, duration_us, results, comparisons, summary) ->
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
