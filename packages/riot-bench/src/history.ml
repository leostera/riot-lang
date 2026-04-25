open Std
open Std.Result.Syntax
open Riot_model

include Model

type run_context = Storage.run_context

let create_run_context = Storage.create_run_context

let run_id = Storage.run_id

let suite_run_path = Storage.suite_run_path

let load_recent_suite_runs = Storage.load_recent_suite_runs

let save_suite_run = Storage.save_suite_run

let history_sample = fun (stored: stored_suite_run) statistics -> { run_id = stored.run_id; partial = stored.partial; statistics }

let benchmark_history = fun previous_runs (current_result: bench_case_result) ->
  match current_result.result with
  | Failed _ | Skipped -> None
  | Completed current ->
      let history = List.filter_map previous_runs ~fn:(
        fun (stored: stored_suite_run) ->
          let previous_result = List.find stored.suite_run.benchmarks ~fn:(
            fun (previous_result: bench_case_result) -> String.equal previous_result.name current_result.name
          ) in
          match previous_result with
          | Some { result = Completed stats; _ } -> Some (history_sample stored stats)
          | Some { result = Failed _; _ } | Some { result = Skipped; _ } | None -> None
      ) in
      if List.is_empty history then
        None
      else
        let baseline = Statistics.baseline_statistics history in
        let current_cv = Statistics.coefficient_of_variation current in
        let baseline_cv = Statistics.coefficient_of_variation baseline in
        Some {
          index = current_result.index;
          name = current_result.name;
          current;
          baseline;
          current_cv;
          baseline_cv;
          stability = Statistics.stability_of_cv ~current_cv ~baseline_cv;
          history
        }

let comparison_case_history = fun previous_runs description (current_case: bench_comparison_case_result) ->
  let history = List.filter_map previous_runs ~fn:(
    fun (stored: stored_suite_run) ->
      let comparison = List.find stored.suite_run.comparisons ~fn:(
        fun (comparison: bench_comparison_result) -> String.equal comparison.description description
      ) in
      match comparison with
      | None -> None
      | Some comparison ->
          let previous_case = List.find comparison.case_results ~fn:(
            fun (previous_case: bench_comparison_case_result) -> String.equal previous_case.name current_case.name
          ) in
          match previous_case with
          | Some previous_case -> Some (history_sample stored previous_case.statistics)
          | None -> None
  ) in
  if List.is_empty history then
    None
  else
    let baseline = Statistics.baseline_statistics history in
    let current_cv = Statistics.coefficient_of_variation current_case.statistics in
    let baseline_cv = Statistics.coefficient_of_variation baseline in
    Some {
      description;
      name = current_case.name;
      current = current_case.statistics;
      baseline;
      current_cv;
      baseline_cv;
      stability = Statistics.stability_of_cv ~current_cv ~baseline_cv;
      history
    }

let compare_suite_run = fun context ~package_name ~suite_name ~(current:suite_run) ~limit ->
  let* previous_runs = Storage.load_recent_suite_runs context ~package_name ~suite_name ~limit
  in
  let benchmarks = List.filter_map current.benchmarks ~fn:(benchmark_history previous_runs) in
  let comparisons = List.flat_map current.comparisons ~fn:(
    fun comparison -> List.filter_map comparison.case_results ~fn:(
      fun current_case -> comparison_case_history previous_runs comparison.description current_case
    )
  ) in Ok { benchmarks; comparisons }
