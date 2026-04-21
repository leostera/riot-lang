open Std
open Std.Result.Syntax
open Riot_model

let ( >>= ) = fun value fn -> Result.and_then value ~fn

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

type suite_summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}

type suite_run = {
  status: int;
  started_at_us: int option;
  completed_at_us: int option;
  duration_us: int option;
  summary: suite_summary;
  benchmarks: bench_case_result list;
  comparisons: bench_comparison_result list;
}

type stored_suite_run = {
  run_id: string;
  package_name: Package_name.t;
  suite_name: string;
  profile: string;
  target: Target.t;
  filter: string option;
  partial: bool;
  git_head: string option;
  git_dirty: bool option;
  argv: string list;
  suite_run: suite_run;
}

type history_sample = {
  run_id: string;
  partial: bool;
  statistics: bench_statistics;
}

type benchmark_history = {
  index: int;
  name: string;
  current: bench_statistics;
  history: history_sample list;
}

type comparison_case_history = {
  description: string;
  name: string;
  current: bench_statistics;
  history: history_sample list;
}

type suite_history = {
  benchmarks: benchmark_history list;
  comparisons: comparison_case_history list;
}

type run_context = {
  run_id: string;
  workspace_root: Path.t;
  profile: string;
  target: Target.t;
  filter: string option;
  partial: bool;
  argv: string list;
  git_head: string option;
  git_dirty: bool option;
}

let sanitize_run_id = fun value ->
  String.map value
    ~fn:(
      function
      | ':'
      | '/'
      | '\\'
      | ' ' -> '-'
      | ch -> ch
    )

let run_id_suffix = fun () ->
  let raw = UUID.to_string_nodash (UUID.v7 ()) in
  if String.length raw > 8 then
    String.sub raw ~offset:0 ~len:8
  else
    raw

let make_run_id = fun () ->
  sanitize_run_id (DateTime.to_iso8601 (DateTime.now_utc ())) ^ "-" ^ run_id_suffix ()

let maybe_trimmed_stdout = fun (output: Command.output) ->
  let trimmed = String.trim output.stdout in
  if String.equal trimmed "" then
    None
  else
    Some trimmed

let read_git_head = fun ~workspace_root ->
  match Command.output
    (Command.make "git" ~cwd:(Path.to_string workspace_root) ~args:[ "rev-parse"; "HEAD" ]) with
  | Ok output when Int.equal output.status 0 -> maybe_trimmed_stdout output
  | _ -> None

let read_git_dirty = fun ~workspace_root ->
  match Command.output
    (Command.make "git" ~cwd:(Path.to_string workspace_root) ~args:[ "status"; "--porcelain" ]) with
  | Ok output when Int.equal output.status 0 -> Some (not
    (String.equal (String.trim output.stdout) ""))
  | _ -> None

let create_run_context = fun ~workspace_root ?target ~profile ~filter ~partial ~argv () ->
  let target = Option.unwrap_or ~default:(Riot_dirs.host_target ()) target in
  {
    run_id = make_run_id ();
    workspace_root;
    profile;
    target;
    filter;
    partial;
    argv;
    git_head = read_git_head ~workspace_root;
    git_dirty = read_git_dirty ~workspace_root;
  }

let run_id = fun context -> context.run_id

let bench_root = fun ~workspace_root ->
  Path.(Riot_dirs.workspace_riot_dir ~workspace_root / Path.v "bench")

let suite_runs_dir = fun ~workspace_root ~package_name ~suite_name ->
  Path.(bench_root ~workspace_root
  / Path.v (Package_name.to_string package_name)
  / Path.v suite_name
  / Path.v "runs")

let suite_run_path = fun context ~package_name ~suite_name ->
  Path.(suite_runs_dir ~workspace_root:context.workspace_root ~package_name ~suite_name
  / Path.v (context.run_id ^ ".json"))

let json_of_option = fun value ~some ->
  match value with
  | Some value -> some value
  | None -> Data.Json.Null

let duration_nanos = fun duration -> Int64.to_int (Time.Duration.to_nanos duration)

let statistics_to_json = fun (stats: bench_statistics) ->
  Data.Json.Object [
    ("min_nanos", Data.Json.Int (duration_nanos stats.min));
    ("max_nanos", Data.Json.Int (duration_nanos stats.max));
    ("mean_nanos", Data.Json.Int (duration_nanos stats.mean));
    ("median_nanos", Data.Json.Int (duration_nanos stats.median));
    ("std_dev_nanos", Data.Json.Int (duration_nanos stats.std_dev));
    ("iterations", Data.Json.Int stats.iterations);
    ("total_time_nanos", Data.Json.Int (duration_nanos stats.total_time));
  ]

let benchmark_to_json = fun (benchmark: bench_case_result) ->
  let base_fields = [
    ("index", Data.Json.Int benchmark.index);
    ("name", Data.Json.String benchmark.name);
  ] in
  match benchmark.result with
  | Completed stats -> Data.Json.Object (base_fields
  @ [ ("status", Data.Json.String "completed"); ("statistics", statistics_to_json stats); ])
  | Failed message -> Data.Json.Object (base_fields
  @ [ ("status", Data.Json.String "failed"); ("message", Data.Json.String message); ])
  | Skipped -> Data.Json.Object (base_fields @ [ ("status", Data.Json.String "skipped") ])

let comparison_to_json = fun (comparison: bench_comparison_result) ->
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
        ~fn:(fun ((name, ratio): string * float) ->
          Data.Json.Object [ ("name", Data.Json.String name); ("ratio", Data.Json.Float ratio); ]))
    );
  ]

let field = fun name fields ->
  List.find fields
    ~fn:(fun (field_name, _) ->
      String.equal field_name name) |> Option.map ~fn:(fun (_, value) -> value)

let rec json_type_name = function
  | Data.Json.Null -> "null"
  | Data.Json.Bool _ -> "bool"
  | Data.Json.Int _ -> "int"
  | Data.Json.Float _ -> "float"
  | Data.Json.String _ -> "string"
  | Data.Json.Array _ -> "array"
  | Data.Json.Object _ -> "object"
  | Data.Json.Embed json -> json_type_name json

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = function
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_string = function
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_bool = function
  | Data.Json.Bool value -> Ok value
  | other -> error_expected "bool" other

let get_int = function
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let get_float = function
  | Data.Json.Float value -> Ok value
  | Data.Json.Int value -> Ok (Float.of_int value)
  | other -> error_expected "float" other

let get_array = function
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_field_required = fun fields name ->
  match field name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let get_optional_string = fun fields name ->
  match field name fields with
  | None
  | Some Data.Json.Null -> Ok None
  | Some value -> get_string value |> Result.map ~fn:Option.some

let get_optional_bool = fun fields name ->
  match field name fields with
  | None
  | Some Data.Json.Null -> Ok None
  | Some value -> get_bool value |> Result.map ~fn:Option.some

let get_optional_int = fun fields name ->
  match field name fields with
  | None
  | Some Data.Json.Null -> Ok None
  | Some value -> get_int value |> Result.map ~fn:Option.some

let rec map_results = fun xs ~fn ->
  match xs with
  | [] -> Ok []
  | value :: rest ->
      let* mapped = fn value in
      let* mapped_rest = map_results rest ~fn in
      Ok (mapped :: mapped_rest)

let statistics_of_json = fun json ->
  let* fields = get_object json in
  let duration_field name =
    let* nanos = get_field_required fields name >>= get_int in
    Ok (Time.Duration.from_nanos nanos)
  in
  let* min = duration_field "min_nanos" in
  let* max = duration_field "max_nanos" in
  let* mean = duration_field "mean_nanos" in
  let* median = duration_field "median_nanos" in
  let* std_dev = duration_field "std_dev_nanos" in
  let* iterations = get_field_required fields "iterations" >>= get_int in
  let* total_time = duration_field "total_time_nanos" in
  Ok {
    min;
    max;
    mean;
    median;
    std_dev;
    iterations;
    total_time;
  }

let bench_case_result_of_json = fun json ->
  let* fields = get_object json in
  let* index = get_field_required fields "index" >>= get_int in
  let* name = get_field_required fields "name" >>= get_string in
  let* status = get_field_required fields "status" >>= get_string in
  let* result =
    match status with
    | "completed" ->
        let* stats = get_field_required fields "statistics" >>= statistics_of_json in
        Ok (Completed stats)
    | "failed" ->
        let* message = get_field_required fields "message" >>= get_string in
        Ok (Failed message)
    | "skipped" ->
        Ok Skipped
    | other ->
        Error ("unknown benchmark status " ^ other)
  in
  Ok { index; name; result }

let comparison_case_result_of_json = fun json ->
  let* fields = get_object json in
  let* name = get_field_required fields "name" >>= get_string in
  let* statistics = get_field_required fields "statistics" >>= statistics_of_json in
  Ok { name; statistics }

let speedup_ratio_of_json = fun json ->
  let* fields = get_object json in
  let* name = get_field_required fields "name" >>= get_string in
  let* ratio = get_field_required fields "ratio" >>= get_float in
  Ok (name, ratio)

let comparison_result_of_json = fun json ->
  let* fields = get_object json in
  let* description = get_field_required fields "description" >>= get_string in
  let* fastest = get_field_required fields "fastest" >>= get_string in
  let* case_results = get_field_required fields "case_results" >>= get_array >>= map_results ~fn:comparison_case_result_of_json in
  let* speedup_ratios = get_field_required fields "speedup_ratios"
  >>= get_array
  >>= map_results ~fn:speedup_ratio_of_json in
  Ok { description; case_results; fastest; speedup_ratios }

let suite_summary_of_json = fun json ->
  let* fields = get_object json in
  let* total = get_field_required fields "total" >>= get_int in
  let* completed = get_field_required fields "completed" >>= get_int in
  let* skipped = get_field_required fields "skipped" >>= get_int in
  let* failed = get_field_required fields "failed" >>= get_int in
  Ok { total; completed; skipped; failed }

let suite_run_of_json = fun json ->
  let* fields = get_object json in
  let* status = get_field_required fields "status" >>= get_int in
  let* started_at_us = get_optional_int fields "started_at_us" in
  let* completed_at_us = get_optional_int fields "completed_at_us" in
  let* duration_us = get_optional_int fields "duration_us" in
  let* summary = get_field_required fields "summary" >>= suite_summary_of_json in
  let* benchmarks = get_field_required fields "benchmarks" >>= get_array >>= map_results ~fn:bench_case_result_of_json in
  let* comparisons = get_field_required fields "comparisons" >>= get_array >>= map_results ~fn:comparison_result_of_json in
  Ok {
    status;
    started_at_us;
    completed_at_us;
    duration_us;
    summary;
    benchmarks;
    comparisons;
  }

let stored_suite_run_of_json = fun json ->
  let* fields = get_object json in
  let* run_id = get_field_required fields "run_id" >>= get_string in
  let* suite_fields = get_field_required fields "suite" >>= get_object in
  let* package_name = get_field_required suite_fields "package"
  >>= get_string
  >>= Riot_model.Package_name.from_string in
  let* suite_name = get_field_required suite_fields "name" >>= get_string in
  let* profile = get_field_required suite_fields "profile" >>= get_string in
  let* target = get_field_required suite_fields "target" >>= get_string >>= Target.from_string in
  let* selection_fields = get_field_required fields "selection" >>= get_object in
  let* filter = get_optional_string selection_fields "filter" in
  let* partial = get_field_required selection_fields "partial" >>= get_bool in
  let* workspace_fields = get_field_required fields "workspace" >>= get_object in
  let* git_head = get_optional_string workspace_fields "git_head" in
  let* git_dirty = get_optional_bool workspace_fields "git_dirty" in
  let* argv =
    let* command_fields = get_field_required fields "command" >>= get_object in
    get_field_required command_fields "argv" >>= get_array >>= map_results ~fn:get_string
  in
  let* suite_run = get_field_required fields "suite_run" >>= suite_run_of_json in
  Ok {
    run_id;
    package_name;
    suite_name;
    profile;
    target;
    filter;
    partial;
    git_head;
    git_dirty;
    argv;
    suite_run;
  }

let read_stored_suite_run = fun path ->
  let* content = Result.map_err (Fs.read_to_string path) ~fn:IO.error_message in
  let* json =
    Result.map_err
      (Data.Json.of_string content)
      ~fn:(fun error -> "invalid json in " ^ Path.to_string path ^ ": " ^ Data.Json.error_to_string error)
  in
  stored_suite_run_of_json json
  |> Result.map_err
    ~fn:(fun error -> "invalid benchmark history file " ^ Path.to_string path ^ ": " ^ error)

let list_suite_run_paths = fun ~workspace_root ~package_name ~suite_name ->
  let dir = suite_runs_dir ~workspace_root ~package_name ~suite_name in
  let* exists = Fs.exists dir |> Result.map_err ~fn:IO.error_message in
  if not exists then
    Ok []
  else
    let* entries = Fs.read_dir dir |> Result.map_err ~fn:IO.error_message in
    let paths =
      entries
      |> Iter.MutIterator.to_list
      |> List.map ~fn:(Path.join dir)
      |> fun paths ->
        List.filter paths ~fn:(fun path -> String.ends_with ~suffix:".json" (Path.basename path))
        |> fun paths ->
          List.sort paths
            ~compare:(fun left right ->
              String.compare (Path.basename right) (Path.basename left))
    in
    Ok paths

let load_recent_suite_runs = fun context ~package_name ~suite_name ~limit ->
  if Int.(limit <= 0) then
    Ok []
  else
    let* paths = list_suite_run_paths ~workspace_root:context.workspace_root ~package_name ~suite_name in
    let rec collect acc remaining = function
      | [] ->
          Ok (List.rev acc)
      | _ when Int.(remaining <= 0) ->
          Ok (List.rev acc)
      | path :: rest ->
          let file_run_id = Path.remove_extension path |> Path.basename in
          if String.equal file_run_id context.run_id then
            collect acc remaining rest
          else
            let* stored = read_stored_suite_run path in
            if
              Package_name.equal stored.package_name package_name
              && String.equal stored.suite_name suite_name
              && String.equal stored.profile context.profile
              && Target.equal stored.target context.target
            then
              collect (stored :: acc) (remaining - 1) rest
            else
              collect acc remaining rest
    in
    collect [] limit paths

let history_sample = fun (stored: stored_suite_run) statistics ->
  { run_id = stored.run_id; partial = stored.partial; statistics }

let benchmark_history = fun previous_runs (current_result: bench_case_result) ->
  match current_result.result with
  | Failed _
  | Skipped -> None
  | Completed current ->
      let history =
        List.filter_map previous_runs
          ~fn:(fun (stored: stored_suite_run) ->
            let previous_result =
              List.find stored.suite_run.benchmarks
                ~fn:(fun (previous_result: bench_case_result) ->
                  String.equal previous_result.name current_result.name)
            in
            match previous_result with
            | Some { result=Completed stats; _ } -> Some (history_sample stored stats)
            | Some { result=Failed _; _ }
            | Some { result=Skipped; _ }
            | None -> None)
      in
      if List.is_empty history then
        None
      else
        Some { index = current_result.index; name = current_result.name; current; history }

let comparison_case_history = fun previous_runs description (
  current_case: bench_comparison_case_result
) ->
  let history =
    List.filter_map previous_runs
      ~fn:(fun (stored: stored_suite_run) ->
        let comparison =
          List.find stored.suite_run.comparisons
            ~fn:(fun (comparison: bench_comparison_result) ->
              String.equal comparison.description description)
        in
        match comparison with
        | None -> None
        | Some comparison ->
            let previous_case =
              List.find comparison.case_results
                ~fn:(fun (previous_case: bench_comparison_case_result) ->
                  String.equal previous_case.name current_case.name)
            in
            (
              match previous_case with
              | Some previous_case -> Some (history_sample stored previous_case.statistics)
              | None -> None
            ))
  in
  if List.is_empty history then
    None
  else
    Some { description; name = current_case.name; current = current_case.statistics; history }

let compare_suite_run = fun context ~package_name ~suite_name ~(current:suite_run) ~limit ->
  let* previous_runs = load_recent_suite_runs context ~package_name ~suite_name ~limit in
  let benchmarks =
    List.filter_map
      current.benchmarks
      ~fn:(fun current_result -> benchmark_history previous_runs current_result)
  in
  let comparisons =
    List.flat_map
      current.comparisons
      ~fn:(fun comparison ->
        List.filter_map
          comparison.case_results
          ~fn:(fun current_case -> comparison_case_history previous_runs comparison.description current_case))
  in
  Ok { benchmarks; comparisons }

let suite_run_to_json = fun context ~package_name ~suite_name (suite_run: suite_run) ->
  Data.Json.Object [
    ("schema_version", Data.Json.Int 1);
    ("run_id", Data.Json.String context.run_id);
    (
      "suite",
      Data.Json.Object [
        ("package", Data.Json.String (Package_name.to_string package_name));
        ("name", Data.Json.String suite_name);
        ("profile", Data.Json.String context.profile);
        ("target", Data.Json.String (Target.to_string context.target));
      ]
    );
    (
      "selection",
      Data.Json.Object [
        ("filter", json_of_option context.filter ~some:(fun value -> Data.Json.String value));
        ("partial", Data.Json.Bool context.partial);
      ]
    );
    (
      "workspace",
      Data.Json.Object [
        ("git_head", json_of_option context.git_head ~some:(fun value -> Data.Json.String value));
        ("git_dirty", json_of_option context.git_dirty ~some:(fun value -> Data.Json.Bool value));
      ]
    );
    (
      "command",
      Data.Json.Object [
        ("argv", Data.Json.Array (List.map context.argv ~fn:(fun value -> Data.Json.String value)));
      ]
    );
    (
      "suite_run",
      Data.Json.Object [
        ("status", Data.Json.Int suite_run.status);
        (
          "started_at_us",
          json_of_option suite_run.started_at_us ~some:(fun value -> Data.Json.Int value)
        );
        (
          "completed_at_us",
          json_of_option suite_run.completed_at_us ~some:(fun value -> Data.Json.Int value)
        );
        (
          "duration_us",
          json_of_option suite_run.duration_us ~some:(fun value -> Data.Json.Int value)
        );
        (
          "summary",
          Data.Json.Object [
            ("total", Data.Json.Int suite_run.summary.total);
            ("completed", Data.Json.Int suite_run.summary.completed);
            ("skipped", Data.Json.Int suite_run.summary.skipped);
            ("failed", Data.Json.Int suite_run.summary.failed);
          ]
        );
        ("benchmarks", Data.Json.Array (List.map suite_run.benchmarks ~fn:benchmark_to_json));
        ("comparisons", Data.Json.Array (List.map suite_run.comparisons ~fn:comparison_to_json));
      ]
    );
  ]

let should_save_suite_run = fun (suite_run: suite_run) ->
  not (List.is_empty suite_run.benchmarks) || not (List.is_empty suite_run.comparisons)

let save_suite_run = fun context ~package_name ~suite_name ~suite_run ->
  if not (should_save_suite_run suite_run) then
    Ok None
  else
    let path = suite_run_path context ~package_name ~suite_name in
    let* () = Fs.create_dir_all (Path.dirname path) |> Result.map_err ~fn:IO.error_message in
    Fs.write
      (Data.Json.to_string_pretty (suite_run_to_json context ~package_name ~suite_name suite_run))
      path
    |> Result.map ~fn:(fun () -> Some path)
    |> Result.map_err ~fn:IO.error_message
