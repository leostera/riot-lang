open Global
open Collections

type timing = {
  iteration: int;
  duration: Time.Duration.t;
}

type gc_stats = Kernel.Gc.quick_stat = {
  minor_collections: int;
  major_collections: int;
  compactions: int;
}

type statistics = {
  min: Time.Duration.t;
  max: Time.Duration.t;
  mean: Time.Duration.t;
  median: Time.Duration.t;
  std_dev: Time.Duration.t;
  iterations: int;
  total_time: Time.Duration.t;
  gc: gc_stats;
}

type bench_result =
  Completed of statistics
  | Failed of string
  | Skipped

type t = {
  index: int;
  name: string;
  result: bench_result;
}

let zero_gc_stats: gc_stats = { minor_collections = 0; major_collections = 0; compactions = 0 }

let make_statistics = fun ?(gc = zero_gc_stats) timings ->
  let durations =
    List.map timings ~fn:(fun t -> t.duration)
  in
  let sorted = List.sort durations ~compare:Time.Duration.compare in
  (* Calculate total time *)
  let total_nanos =
    List.fold_left durations ~init:0L
      ~fn:(fun acc d ->
        Int64.add acc (Time.Duration.to_nanos d))
  in
  let iterations = List.length timings in
  let mean_nanos = Int64.div total_nanos (Int64.from_int iterations) in
  (* Min and max *)
  let min = List.get sorted ~at:0 |> Option.unwrap in
  let max = List.get sorted ~at:(List.length sorted - 1) |> Option.unwrap in
  (* Median *)
  let median = List.get sorted ~at:(iterations / 2) |> Option.unwrap in
  (* Standard deviation *)
  let variance =
    List.fold_left durations ~init:0.0
      ~fn:(fun acc d ->
        let diff = Int64.sub (Time.Duration.to_nanos d) mean_nanos in
        let diff_f = Int64.to_float diff in
        acc +. (diff_f *. diff_f))
  in
  let std_dev_nanos = Float.sqrt (variance /. Float.from_int iterations) in
  {
    min;
    max;
    mean = Time.Duration.from_nanos (Int64.to_int mean_nanos);
    median;
    std_dev = Time.Duration.from_nanos (Int.from_float std_dev_nanos);
    iterations;
    total_time = Time.Duration.from_nanos (Int64.to_int total_nanos);
    gc;
  }

type summary = {
  total: int;
  completed: int;
  skipped: int;
  failed: int;
}

let make_summary = fun results ->
  List.fold_left results ~init:{
    total = List.length results;
    completed = 0;
    skipped = 0;
    failed = 0
  }
    ~fn:(fun acc (result: t) ->
      match result.result with
      | Completed _ -> { acc with completed = acc.completed + 1 }
      | Skipped -> { acc with skipped = acc.skipped + 1 }
      | Failed _ -> { acc with failed = acc.failed + 1 })

(* Comparison results *)

type case_result = {
  name: string;
  statistics: statistics;
}

type comparison_result = {
  description: string;
  case_results: case_result list;
  fastest: string;
  speedup_ratios: (string * float) list;
}

let find_fastest = fun results ->
  let compare_by_mean a b = Time.Duration.compare a.statistics.mean b.statistics.mean in
  let sorted = List.sort results ~compare:compare_by_mean in
  List.get sorted ~at:0 |> Option.unwrap

let calculate_speedup = fun fastest_mean other_mean ->
  let fastest_ns = Time.Duration.to_nanos fastest_mean in
  let other_ns = Time.Duration.to_nanos other_mean in
  Int64.to_float other_ns /. Int64.to_float fastest_ns

let make_comparison_result = fun description case_results ->
  let fastest = find_fastest case_results in
  let speedup_ratios =
    List.map case_results
      ~fn:(fun res ->
        if res.name = fastest.name then
          (res.name, 1.0)
        else
          let ratio = calculate_speedup fastest.statistics.mean res.statistics.mean in
          (res.name, ratio))
  in
  { description; case_results; fastest = fastest.name; speedup_ratios }
