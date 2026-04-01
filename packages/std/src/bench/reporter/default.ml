open Global
open Collections

let init = fun (info: Intf.suite_info) count ->
  println "";
  println ("Running " ^ string_of_int count ^ " benchmarks from " ^ info.name ^ "...");
  println ""

let on_result = fun index (result: Bench_result.t) ->
  match result.result with
  | Bench_result.Completed stats ->
      println ("[" ^ string_of_int index ^ "] " ^ result.name ^ ":");
      println ("  iterations: " ^ string_of_int stats.iterations);
      println ("  mean:       " ^ Time.Duration.to_secs_string ~precision:6 stats.mean);
      println ("  median:     " ^ Time.Duration.to_secs_string ~precision:6 stats.median);
      println ("  min:        " ^ Time.Duration.to_secs_string ~precision:6 stats.min);
      println ("  max:        " ^ Time.Duration.to_secs_string ~precision:6 stats.max);
      println ("  std_dev:    " ^ Time.Duration.to_secs_string ~precision:6 stats.std_dev);
      println ""
  | Bench_result.Skipped ->
      println ("[" ^ string_of_int index ^ "] " ^ result.name ^ ": SKIPPED");
      println ""
  | Bench_result.Failed msg ->
      println ("[" ^ string_of_int index ^ "] " ^ result.name ^ ": FAILED");
      println ("  Error: " ^ msg);
      println ""

let finalize = fun (summary: Bench_result.summary) ->
  println
    ("Summary: "
    ^ string_of_int summary.total
    ^ " total, "
    ^ string_of_int summary.completed
    ^ " completed, "
    ^ string_of_int summary.skipped
    ^ " skipped, "
    ^ string_of_int summary.failed
    ^ " failed")

(* Comparison reporting *)

let on_comparison_start = fun index description count ->
  println "";
  println ("[" ^ string_of_int index ^ "] Comparison: " ^ description);
  println ("  Running " ^ string_of_int count ^ " implementations...");
  println ""

let on_comparison_case_result = fun index name (stats: Bench_result.statistics) ->
  println ("  [" ^ string_of_int index ^ "] " ^ name ^ ":");
  println ("    iterations: " ^ string_of_int stats.iterations);
  println
    ("    mean:       "
    ^ Time.Duration.to_secs_string ~precision:6 stats.mean
    ^ " ± "
    ^ Time.Duration.to_secs_string ~precision:6 stats.std_dev);
  println ("    min:        " ^ Time.Duration.to_secs_string ~precision:6 stats.min);
  println ("    max:        " ^ Time.Duration.to_secs_string ~precision:6 stats.max);
  println ""

let on_comparison_summary = fun (result: Bench_result.comparison_result) ->
  println "  Summary:";
  let fastest_name = result.fastest in
  (* Show each case relative to fastest *)
  List.iter
    (fun ((name, ratio)) ->
      if not (String.equal name fastest_name) then
        let speedup_str = Float.to_string ~precision:2 ratio in
        let pct_slower = (ratio -. 1.0) *. 100.0 in
        let pct_str = Float.to_string ~precision:1 pct_slower in
        println ("    " ^ fastest_name ^ " ran " ^ speedup_str ^ "x faster than " ^ name);
        println ("      (" ^ pct_str ^ "% slower)"))
    result.speedup_ratios;
  println ""
