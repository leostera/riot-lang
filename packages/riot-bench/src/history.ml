open Std
open Std.Result.Syntax
open Riot_model

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

let suite_run_path = fun context ~package_name ~suite_name ->
  Path.(bench_root ~workspace_root:context.workspace_root
  / Path.v (Package_name.to_string package_name)
  / Path.v suite_name
  / Path.v "runs"
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
  not (List.is_empty suite_run.benchmarks)
  || not (List.is_empty suite_run.comparisons)

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
