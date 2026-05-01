open Std
open Riot_model

type gc_stats = { minor_collections: int; major_collections: int; compactions: int }

type bench_statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
  gc: gc_stats;
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

type suite_summary = { total: int; completed: int; skipped: int; failed: int }

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

type stability =
  | Stable
  | Noisy

type benchmark_history = {
  index: int;
  name: string;
  current: bench_statistics;
  baseline: bench_statistics;
  current_cv: float option;
  baseline_cv: float option;
  stability: stability;
  history: history_sample list;
}

type comparison_case_history = {
  description: string;
  name: string;
  current: bench_statistics;
  baseline: bench_statistics;
  current_cv: float option;
  baseline_cv: float option;
  stability: stability;
  history: history_sample list;
}

type suite_history = {
  benchmarks: benchmark_history list;
  comparisons: comparison_case_history list;
}
