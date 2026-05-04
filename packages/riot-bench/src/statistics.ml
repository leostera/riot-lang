open Std
open Model

let duration_to_nanos_float = fun duration ->
  Time.Duration.to_nanos duration
  |> Int64.to_float

let float_median = fun values ->
  match List.sort values ~compare:Float.compare with
  | [] -> None
  | sorted ->
      let len = List.length sorted in
      let mid = len / 2 in
      if Int.equal (len mod 2) 1 then
        List.get sorted ~at:mid
      else
        match (List.get sorted ~at:(mid - 1), List.get sorted ~at:mid) with
        | (Some left, Some right) -> Some ((left +. right) /. 2.0)
        | _ -> None

let duration_median = fun durations ->
  durations
  |> List.map ~fn:duration_to_nanos_float
  |> float_median
  |> Option.map ~fn:(fun nanos -> Time.Duration.from_secs_float (nanos /. 1_000_000_000.0))

let int_median = fun values ->
  values
  |> List.map ~fn:Float.from_int
  |> float_median
  |> Option.map ~fn:Float.to_int

let coefficient_of_variation = fun (stats: bench_statistics) ->
  let mean = duration_to_nanos_float stats.mean in
  if Float.equal mean 0.0 then
    None
  else
    Some (duration_to_nanos_float stats.std_dev /. mean)

let stability_of_cv = fun ~current_cv ~baseline_cv ->
  match current_cv with
  | None -> Noisy
  | Some current_cv ->
      let threshold =
        match baseline_cv with
        | None -> 0.05
        | Some baseline_cv ->
            let scaled = baseline_cv *. 2.0 in
            if Float.compare scaled 0.05 = Order.GT then
              scaled
            else
              0.05
      in
      if Float.compare current_cv threshold != Order.GT then
        Stable
      else
        Noisy

let baseline_statistics = fun (history: history_sample list) ->
  let statistics = List.map history ~fn:(fun sample -> sample.statistics) in
  let duration_field project name =
    project statistics
    |> duration_median
    |> Option.expect ~msg:("expected non-empty history for " ^ name)
  in
  let iterations =
    statistics
    |> List.map ~fn:(fun stats -> stats.iterations)
    |> int_median
    |> Option.expect ~msg:"expected non-empty history for iterations"
  in
  let gc_field project name =
    project statistics
    |> List.map ~fn:Float.from_int
    |> float_median
    |> Option.map ~fn:Float.to_int
    |> Option.expect ~msg:("expected non-empty history for " ^ name)
  in
  {
    min = duration_field (List.map ~fn:(fun stats -> stats.min)) "min";
    max = duration_field (List.map ~fn:(fun stats -> stats.max)) "max";
    mean = duration_field (List.map ~fn:(fun stats -> stats.mean)) "mean";
    median = duration_field (List.map ~fn:(fun stats -> stats.median)) "median";
    std_dev = duration_field (List.map ~fn:(fun stats -> stats.std_dev)) "std_dev";
    iterations;
    total_time = duration_field (List.map ~fn:(fun stats -> stats.total_time)) "total_time";
    gc = {
      minor_collections = gc_field
        (List.map ~fn:(fun stats -> stats.gc.minor_collections))
        "gc.minor_collections";
      major_collections = gc_field
        (List.map ~fn:(fun stats -> stats.gc.major_collections))
        "gc.major_collections";
      compactions = gc_field (List.map ~fn:(fun stats -> stats.gc.compactions)) "gc.compactions";
    };
  }
