open Global
open Collections

let print_gc = fun ~indent (gc: Bench_result.gc_stats) ->
  println (indent ^ "gc.minor:   " ^ Int.to_string gc.minor_collections);
  println (indent ^ "gc.major:   " ^ Int.to_string gc.major_collections);
  println (indent ^ "gc.compact: " ^ Int.to_string gc.compactions)

let init = fun (info: Intf.suite_info) count ->
  println "";
  println ("Running " ^ Int.to_string count ^ " benchmarks from " ^ info.name ^ "...");
  println ""

let on_case_start = fun _index _name ~iterations:_ ~warmup:_ -> ()

let on_result = fun index (result: Bench_result.t) ->
  match result.result with
  | Bench_result.Completed stats ->
      println ("[" ^ Int.to_string index ^ "] " ^ result.name ^ ":");
      println ("  iterations: " ^ Int.to_string stats.iterations);
      println ("  mean:       " ^ Time.Duration.to_secs_string ~precision:6 stats.mean);
      println ("  median:     " ^ Time.Duration.to_secs_string ~precision:6 stats.median);
      println ("  min:        " ^ Time.Duration.to_secs_string ~precision:6 stats.min);
      println ("  max:        " ^ Time.Duration.to_secs_string ~precision:6 stats.max);
      println ("  std_dev:    " ^ Time.Duration.to_secs_string ~precision:6 stats.std_dev);
      print_gc ~indent:"  " stats.gc;
      println ""
  | Bench_result.Skipped ->
      println ("[" ^ Int.to_string index ^ "] " ^ result.name ^ ": SKIPPED");
      println ""
  | Bench_result.Failed msg ->
      println ("[" ^ Int.to_string index ^ "] " ^ result.name ^ ": FAILED");
      println ("  Error: " ^ msg);
      println ""

let finalize = fun (summary: Bench_result.summary) ->
  println
    ("Summary: "
    ^ Int.to_string summary.total
    ^ " total, "
    ^ Int.to_string summary.completed
    ^ " completed, "
    ^ Int.to_string summary.skipped
    ^ " skipped, "
    ^ Int.to_string summary.failed
    ^ " failed")

(* Comparison reporting *)

let on_comparison_start = fun index description count ->
  println "";
  println ("[" ^ Int.to_string index ^ "] Comparison: " ^ description);
  println ("  Running " ^ Int.to_string count ^ " implementations...");
  println ""

let on_comparison_case_result = fun index name (stats: Bench_result.statistics) ->
  println ("  [" ^ Int.to_string index ^ "] " ^ name ^ ":");
  println ("    iterations: " ^ Int.to_string stats.iterations);
  println
    ("    mean:       "
    ^ Time.Duration.to_secs_string ~precision:6 stats.mean
    ^ " ± "
    ^ Time.Duration.to_secs_string ~precision:6 stats.std_dev);
  println ("    min:        " ^ Time.Duration.to_secs_string ~precision:6 stats.min);
  println ("    max:        " ^ Time.Duration.to_secs_string ~precision:6 stats.max);
  print_gc ~indent:"    " stats.gc;
  println ""

let on_comparison_summary = fun (result: Bench_result.comparison_result) ->
  println "  Summary:";
  let fastest_name = result.fastest in
  (* Show each case relative to fastest *)
  List.for_each
    result.speedup_ratios
    ~fn:(fun (name, ratio) ->
      if not (String.equal name fastest_name) then
        (
          let speedup_str = Float.to_string ~precision:2 ratio in
          let pct_slower = (ratio -. 1.0) *. 100.0 in
          let pct_str = Float.to_string ~precision:1 pct_slower in
          println ("    " ^ fastest_name ^ " ran " ^ speedup_str ^ "x faster than " ^ name);
          println ("      (" ^ pct_str ^ "% slower)")
        ));
  println ""
