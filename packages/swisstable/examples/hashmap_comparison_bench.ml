open Std
open Std.Collections

(* Comparison benchmarks: Swisstable vs HashMap *)

(* Insert benchmarks *)

let bench_hashmap_insert_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_swisstable_insert_100 = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done

let bench_hashmap_insert_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_swisstable_insert_10k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done

let bench_hashmap_insert_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_swisstable_insert_100k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done

(* Lookup benchmarks *)

let bench_hashmap_get_from_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "key_5000" in
  ()

let bench_swisstable_get_from_10k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  let _ = Swisstable.get map "key_5000" in
  ()

let bench_hashmap_get_from_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "key_50000" in
  ()

let bench_swisstable_get_from_100k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  let _ = Swisstable.get map "key_50000" in
  ()

(* Missing key lookup *)

let bench_hashmap_get_missing = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "missing_key" in
  ()

let bench_swisstable_get_missing = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  let _ = Swisstable.get map "missing_key" in
  ()

(* Iteration benchmarks *)

let bench_hashmap_iter_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  HashMap.iter (fun _k _v -> ()) map

let bench_swisstable_iter_10k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  Swisstable.iter (fun _k _v -> ()) map

let bench_hashmap_iter_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  HashMap.iter (fun _k _v -> ()) map

let bench_swisstable_iter_100k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  Swisstable.iter (fun _k _v -> ()) map

(* Remove benchmarks *)

let bench_hashmap_remove_from_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.remove map "key_5000" in
  ()

let bench_swisstable_remove_from_10k = fun () ->
  let map = Swisstable.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = Swisstable.insert map key i in
    ()
  done;
  let _ = Swisstable.remove map "key_5000" in
  ()

let benchmarks =
  Bench.[
    compare
      "insert 100 items"
      [
        make_case "HashMap" bench_hashmap_insert_100;
        make_case "Swisstable" bench_swisstable_insert_100;
      ];
    compare_with_config
      ~config:{ iterations = 10; warmup = 2 }
      "insert 10k items"
      [
        make_case "HashMap" bench_hashmap_insert_10k;
        make_case "Swisstable" bench_swisstable_insert_10k;
      ];
    compare_with_config
      ~config:{ iterations = 5; warmup = 1 }
      "insert 100k items"
      [
        make_case "HashMap" bench_hashmap_insert_100k;
        make_case "Swisstable" bench_swisstable_insert_100k;
      ];
    compare_with_config
      ~config:{ iterations = 50; warmup = 5 }
      "get from 10k items"
      [
        make_case "HashMap" bench_hashmap_get_from_10k;
        make_case "Swisstable" bench_swisstable_get_from_10k;
      ];
    compare_with_config
      ~config:{ iterations = 20; warmup = 2 }
      "get from 100k items"
      [
        make_case "HashMap" bench_hashmap_get_from_100k;
        make_case "Swisstable" bench_swisstable_get_from_100k;
      ];
    compare_with_config
      ~config:{ iterations = 50; warmup = 5 }
      "get missing key from 10k items"
      [
        make_case "HashMap" bench_hashmap_get_missing;
        make_case "Swisstable" bench_swisstable_get_missing;
      ];
    compare_with_config
      ~config:{ iterations = 10; warmup = 2 }
      "iterate over 10k items"
      [
        make_case "HashMap" bench_hashmap_iter_10k;
        make_case "Swisstable" bench_swisstable_iter_10k;
      ];
    compare_with_config
      ~config:{ iterations = 5; warmup = 1 }
      "iterate over 100k items"
      [
        make_case "HashMap" bench_hashmap_iter_100k;
        make_case "Swisstable" bench_swisstable_iter_100k;
      ];
    compare_with_config
      ~config:{ iterations = 50; warmup = 5 }
      "remove from 10k items"
      [
        make_case "HashMap" bench_hashmap_remove_from_10k;
        make_case "Swisstable" bench_swisstable_remove_from_10k;
      ];
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"HashMap vs Swisstable Performance" ~benchmarks ~args)
    ~args:Env.args
    ()
