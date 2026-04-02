open Std
open Std.Bench
module HashMap = Kernel.Collections.HashMap
module Cell = Kernel.Sync.Cell

(* Benchmark configuration for very large datasets *)

let large_config = { iterations = 10; warmup = 2 }

let xlarge_config = { iterations = 3; warmup = 1 }

(* Reduced for 1m items *)

(* ========================================================================
 * 1m Item Benchmarks
 * ======================================================================== *)

(* HashMap: Insert 1m items *)

let bench_hashmap_insert_1m = fun () ->
  let map = HashMap.create () in
  for i = 0 to 999_999 do
    ignore (HashMap.insert map i (i * 2))
  done

(* Swisstable: Insert 1m items *)

let bench_swisstable_insert_1m = fun () ->
  let map = Swisstable.with_capacity 1_000_000 in
  for i = 0 to 999_999 do
    ignore (Swisstable.insert map i (i * 2))
  done

(* HashMap: Get from 1m items (10k lookups) *)

let bench_hashmap_get_from_1m = fun () ->
  let map = HashMap.create () in
  for i = 0 to 999_999 do
    ignore (HashMap.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (HashMap.get map (i * 100))
  done

(* Swisstable: Get from 1m items (10k lookups) *)

let bench_swisstable_get_from_1m = fun () ->
  let map = Swisstable.with_capacity 1_000_000 in
  for i = 0 to 999_999 do
    ignore (Swisstable.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (Swisstable.get map (i * 100))
  done

(* HashMap: Get missing keys from 1m items *)

let bench_hashmap_get_missing_from_1m = fun () ->
  let map = HashMap.create () in
  for i = 0 to 999_999 do
    ignore (HashMap.insert map i (i * 2))
  done;
  for i = 1_000_000 to 1_009_999 do
    ignore (HashMap.get map i)
  done

(* Swisstable: Get missing keys from 1m items *)

let bench_swisstable_get_missing_from_1m = fun () ->
  let map = Swisstable.with_capacity 1_000_000 in
  for i = 0 to 999_999 do
    ignore (Swisstable.insert map i (i * 2))
  done;
  for i = 1_000_000 to 1_009_999 do
    ignore (Swisstable.get map i)
  done

(* HashMap: Iterate over 1m items *)

let bench_hashmap_iterate_1m = fun () ->
  let map = HashMap.create () in
  for i = 0 to 999_999 do
    ignore (HashMap.insert map i (i * 2))
  done;
  let sum = Cell.create 0 in
  HashMap.iter
    (fun _k v ->
      Cell.set sum (Cell.get sum + v))
    map

(* Swisstable: Iterate over 1m items *)

let bench_swisstable_iterate_1m = fun () ->
  let map = Swisstable.with_capacity 1_000_000 in
  for i = 0 to 999_999 do
    ignore (Swisstable.insert map i (i * 2))
  done;
  let sum = Cell.create 0 in
  Swisstable.iter
    (fun _k v ->
      Cell.set sum (Cell.get sum + v))
    map

(* HashMap: Remove from 1m items (10k removals) *)

let bench_hashmap_remove_from_1m = fun () ->
  let map = HashMap.create () in
  for i = 0 to 999_999 do
    ignore (HashMap.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (HashMap.remove map (i * 100))
  done

(* Swisstable: Remove from 1m items (10k removals) *)

let bench_swisstable_remove_from_1m = fun () ->
  let map = Swisstable.with_capacity 1_000_000 in
  for i = 0 to 999_999 do
    ignore (Swisstable.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (Swisstable.remove map (i * 100))
  done

(* ========================================================================
 * 500K Item Benchmarks (intermediate size)
 * ======================================================================== *)

(* HashMap: Insert 500k items *)

let bench_hashmap_insert_500k = fun () ->
  let map = HashMap.create () in
  for i = 0 to 499_999 do
    ignore (HashMap.insert map i (i * 2))
  done

(* Swisstable: Insert 500k items *)

let bench_swisstable_insert_500k = fun () ->
  let map = Swisstable.with_capacity 500_000 in
  for i = 0 to 499_999 do
    ignore (Swisstable.insert map i (i * 2))
  done

(* HashMap: Get from 500k items *)

let bench_hashmap_get_from_500k = fun () ->
  let map = HashMap.create () in
  for i = 0 to 499_999 do
    ignore (HashMap.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (HashMap.get map (i * 50))
  done

(* Swisstable: Get from 500k items *)

let bench_swisstable_get_from_500k = fun () ->
  let map = Swisstable.with_capacity 500_000 in
  for i = 0 to 499_999 do
    ignore (Swisstable.insert map i (i * 2))
  done;
  for i = 0 to 9_999 do
    ignore (Swisstable.get map (i * 50))
  done

(* ========================================================================
 * Main benchmark suite
 * ======================================================================== *)

let benchmarks =
  Bench.[
    compare_with_config
      ~config:large_config
      "insert 500k items"
      [
        make_case "HashMap" bench_hashmap_insert_500k;
        make_case "Swisstable" bench_swisstable_insert_500k;
      ];
    compare_with_config
      ~config:large_config
      "get from 500k items"
      [
        make_case "HashMap" bench_hashmap_get_from_500k;
        make_case "Swisstable" bench_swisstable_get_from_500k;
      ];
    compare_with_config
      ~config:xlarge_config
      "insert 1m items"
      [
        make_case "HashMap" bench_hashmap_insert_1m;
        make_case "Swisstable" bench_swisstable_insert_1m;
      ];
    compare_with_config
      ~config:xlarge_config
      "get from 1m items (10k lookups)"
      [
        make_case "HashMap" bench_hashmap_get_from_1m;
        make_case "Swisstable" bench_swisstable_get_from_1m;
      ];
    compare_with_config
      ~config:xlarge_config
      "get missing keys from 1m items"
      [
        make_case "HashMap" bench_hashmap_get_missing_from_1m;
        make_case "Swisstable" bench_swisstable_get_missing_from_1m;
      ];
    compare_with_config
      ~config:xlarge_config
      "iterate over 1m items"
      [
        make_case "HashMap" bench_hashmap_iterate_1m;
        make_case "Swisstable" bench_swisstable_iterate_1m;
      ];
    compare_with_config
      ~config:xlarge_config
      "remove from 1m items (10k removals)"
      [
        make_case "HashMap" bench_hashmap_remove_from_1m;
        make_case "Swisstable" bench_swisstable_remove_from_1m;
      ];
  ]

let () =
  println "HashMap vs Swisstable - Large Dataset Performance\n";
  Actors.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"HashMap vs Swisstable - Large Datasets" ~benchmarks ~args)
    ~args:Env.args
    ()
