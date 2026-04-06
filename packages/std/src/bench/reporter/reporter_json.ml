open Global
open Collections

let benchmark_results = ref []

let comparison_results = ref []

let suite_started_monotonic = ref None

let duration_us = fun duration -> Time.Duration.to_micros duration

let reset = fun () ->
  benchmark_results := [];
  comparison_results := [];
  suite_started_monotonic := Some (Time.Instant.now ())

let statistics_to_json = fun (stats: Bench_result.statistics) ->
  let open Data.Json in obj
    [
      ("min_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.min)));
      ("max_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.max)));
      ("mean_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.mean)));
      ("median_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.median)));
      ("std_dev_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.std_dev)));
      ("iterations", int stats.iterations);
      ("total_time_nanos", int (Int64.to_int (Time.Duration.to_nanos stats.total_time)));
    ]

let benchmark_result_to_json = fun (result: Bench_result.t) ->
  let open Data.Json in
    let base_fields = [ ("index", int result.index); ("name", string result.name) ] in
    match result.result with
    | Bench_result.Completed stats -> obj
      (base_fields @ [ ("status", string "completed"); ("statistics", statistics_to_json stats); ])
    | Bench_result.Failed message -> obj
      (base_fields @ [ ("status", string "failed"); ("message", string message); ])
    | Bench_result.Skipped -> obj (base_fields @ [ ("status", string "skipped") ])

let comparison_result_to_json = fun (result: Bench_result.comparison_result) ->
  let open Data.Json in obj
    [
      ("description", string result.description);
      ("fastest", string result.fastest);
      (
        "case_results",
        array
          (List.map
            (fun (case_result: Bench_result.case_result) ->
              obj
                [
                  ("name", string case_result.name);
                  ("statistics", statistics_to_json case_result.statistics);
                ])
            result.case_results)
      );
      (
        "speedup_ratios",
        array
          (List.map
            (fun (name, ratio) -> obj [ ("name", string name); ("ratio", float ratio); ])
            result.speedup_ratios)
      );
    ]

let init = fun (_suite: Intf.suite_info) _count -> reset ()

let on_result = fun _index result -> benchmark_results := result :: !benchmark_results

let finalize = fun (summary: Bench_result.summary) ->
  let open Data.Json in
    let duration_us =
      match !suite_started_monotonic with
      | Some start -> Time.Instant.elapsed start |> duration_us
      | None -> 0
    in
    let benchmarks = !benchmark_results |> List.rev |> List.map benchmark_result_to_json in
    let comparisons = !comparison_results |> List.rev |> List.map comparison_result_to_json in
    let summary_json = obj
      [
        ("total", int summary.total);
        ("completed", int summary.completed);
        ("skipped", int summary.skipped);
        ("failed", int summary.failed);
      ] in
    let output = obj
      [
        ("benchmarks", array benchmarks);
        ("comparisons", array comparisons);
        ("summary", summary_json);
        ("started_at_us", int 0);
        ("completed_at_us", int duration_us);
        ("duration_us", int duration_us);
      ] in
    println (to_string output)

let on_comparison_start = fun _index _description _count -> ()

let on_comparison_case_result = fun _index _name _stats -> ()

let on_comparison_summary = fun result -> comparison_results := result :: !comparison_results
