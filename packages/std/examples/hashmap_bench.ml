open Std
open Std.Collections

let populate_map = fun count ->
  let map = HashMap.create () in
  for i = 1 to count do
    let key = "key_" ^ Int.to_string i in
    let _ = HashMap.insert map ~key ~value:i in
    ()
  done;
  map

(* Benchmark: Insert operations *)

let bench_insert_100 = fun () ->
  let _ = populate_map 100 in
  ()

let bench_insert_10k = fun () ->
  let _ = populate_map 10_000 in
  ()

let bench_insert_100k = fun () ->
  let _ = populate_map 100_000 in
  ()

let bench_insert_1m = fun () ->
  let _ = populate_map 1_000_000 in
  ()

(* Benchmark: Lookup operations - single lookup from populated map *)

let bench_get_from_100 = fun () ->
  let map = populate_map 100 in
  (* Benchmark: single lookup *)
  let _ = HashMap.get map ~key:"key_50" in
  ()

let bench_get_from_10k = fun () ->
  let map = populate_map 10_000 in
  let _ = HashMap.get map ~key:"key_5000" in
  ()

let bench_get_from_100k = fun () ->
  let map = populate_map 100_000 in
  let _ = HashMap.get map ~key:"key_50000" in
  ()

let bench_get_from_1m = fun () ->
  let map = populate_map 1_000_000 in
  let _ = HashMap.get map ~key:"key_500000" in
  ()

let bench_get_missing = fun () ->
  let map = populate_map 100_000 in
  (* Benchmark: lookup non-existing key *)
  let _ = HashMap.get map ~key:"missing_key" in
  ()

(* Benchmark: Remove operations - single remove from populated map *)

let bench_remove_from_100 = fun () ->
  let map = populate_map 100 in
  let _ = HashMap.remove map ~key:"key_50" in
  ()

let bench_remove_from_10k = fun () ->
  let map = populate_map 10_000 in
  let _ = HashMap.remove map ~key:"key_5000" in
  ()

let bench_remove_from_100k = fun () ->
  let map = populate_map 100_000 in
  let _ = HashMap.remove map ~key:"key_50000" in
  ()

(* Benchmark: Iteration *)

let bench_iter_100 = fun () ->
  let map = populate_map 100 in
  HashMap.for_each map ~fn:(fun _k _v -> ())

let bench_iter_10k = fun () ->
  let map = populate_map 10_000 in
  HashMap.for_each map ~fn:(fun _k _v -> ())

let bench_iter_100k = fun () ->
  let map = populate_map 100_000 in
  HashMap.for_each map ~fn:(fun _k _v -> ())

(* Benchmark: key membership check *)

let bench_has_key_from_100 = fun () ->
  let map = populate_map 100 in
  let _ = HashMap.has_key map ~key:"key_50" in
  ()

let bench_has_key_from_10k = fun () ->
  let map = populate_map 10_000 in
  let _ = HashMap.has_key map ~key:"key_5000" in
  ()

let bench_has_key_from_100k = fun () ->
  let map = populate_map 100_000 in
  let _ = HashMap.has_key map ~key:"key_50000" in
  ()

let benchmarks =
  Bench.[
    case "insert: 100 items" bench_insert_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "insert: 10k items" bench_insert_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "insert: 100k items" bench_insert_100k;
    with_config ~config:{ iterations = 3; warmup = 1 } "insert: 1M items" bench_insert_1m;
    case "get: from 100 items" bench_get_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "get: from 10k items" bench_get_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: from 100k items" bench_get_from_100k;
    with_config ~config:{ iterations = 10; warmup = 2 } "get: from 1M items" bench_get_from_1m;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: missing from 100k" bench_get_missing;
    case "remove: from 100 items" bench_remove_from_100;
    with_config
      ~config:{ iterations = 50; warmup = 5 }
      "remove: from 10k items"
      bench_remove_from_10k;
    with_config
      ~config:{ iterations = 20; warmup = 2 }
      "remove: from 100k items"
      bench_remove_from_100k;
    case "iter: 100 items" bench_iter_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "iter: 10k items" bench_iter_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "iter: 100k items" bench_iter_100k;
    case "has_key: from 100 items" bench_has_key_from_100;
    with_config
      ~config:{ iterations = 50; warmup = 5 }
      "has_key: from 10k items"
      bench_has_key_from_10k;
    with_config
      ~config:{ iterations = 20; warmup = 2 }
      "has_key: from 100k items"
      bench_has_key_from_100k;
  ]

let main ~args = Bench.Cli.main ~name:"HashMap Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
