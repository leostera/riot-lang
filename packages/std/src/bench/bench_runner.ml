open Global
open Collections

type config = {
  reporter: (module Reporter.Intf.Intf);
  suite_info: Reporter.Intf.suite_info;
}

type run_summary = Bench_result.summary

let run_single_benchmark = fun index (bench: Bench_case.t) ->
  if bench.skip then
    Bench_result.{ index; name = bench.name; result = Skipped }
  else
    try
      for _i = 1 to bench.config.warmup do
        bench.fn ()
      done;
      (* Measurement phase *)
      let timings = ref [] in
      for i = 1 to bench.config.iterations do
        let start = Time.Instant.now () in
        bench.fn ();
        let finish = Time.Instant.now () in
        let duration = Time.Instant.duration_since ~earlier:start finish in
        timings := Bench_result.{ iteration = i; duration } :: !timings
      done;
      (* Calculate statistics *)
      let stats = Bench_result.make_statistics (List.rev !timings) in
      Bench_result.{ index; name = bench.name; result = Completed stats }
    with
    | exn ->
        let msg = Exception.to_string exn in
        Bench_result.{ index; name = bench.name; result = Failed msg }

(* Run a comparison benchmark *)

let run_comparison = fun index ((module R : Reporter.Intf.Intf)) (comp: Bench_comparison.t) ->
  R.on_comparison_start index comp.description (List.length comp.cases);
  (* Run each case and collect results *)
  let case_results =
    List.mapi
      (fun i case ->
        (* Reuse run_single_benchmark but ignore the index *)
        let result = run_single_benchmark (i + 1) case in
        match result.result with
        | Bench_result.Completed stats ->
            (* Report immediately *)
            R.on_comparison_case_result (i + 1) case.name stats;
            Some { Bench_result.name = case.name; statistics = stats }
        | Bench_result.Failed msg ->
            println ("    [" ^ string_of_int (i + 1) ^ "] " ^ case.name ^ ": FAILED");
            println ("      Error: " ^ msg);
            None
        | Bench_result.Skipped ->
            println ("    [" ^ string_of_int (i + 1) ^ "] " ^ case.name ^ ": SKIPPED");
            None)
      comp.cases
  in
  let valid_results =
    List.filter_map (fun x -> x) case_results
  in
  (* Create and report comparison summary *)
  if List.length valid_results >= 2 then
    begin
      let comp_result = Bench_result.make_comparison_result comp.description valid_results in
      R.on_comparison_summary comp_result
    end
  else
    println "  (not enough valid results for comparison)"

type bench_item =
  | Single of Bench_case.t
  | Compare of Bench_comparison.t

let run_benchmarks = fun ~config benchmarks ->
  let module R = (val config.reporter : Reporter.Intf.Intf) in
  R.init config.suite_info (List.length benchmarks);
  let results = ref [] in
  let global_index = ref 0 in
  List.iter
    (fun item ->
      match item with
      | Single bench ->
          global_index := !global_index + 1;
          let result = run_single_benchmark !global_index bench in
          R.on_result result.index result;
          results := result :: !results
      | Compare comp ->
          global_index := !global_index + 1;
          run_comparison !global_index (module R) comp)
    benchmarks;
  let summary = Bench_result.make_summary (List.rev !results) in
  R.finalize summary;
  summary
