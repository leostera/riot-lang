open Std
open Std.Collections

module ConcurrentHashMap = Collections.ConcurrentHashMap
module Iterator = Iter.Iterator
module MutIterator = Iter.MutIterator

type op_size = {
  label: string;
  count: int;
  config: Bench.bench_config;
}

let small_config: Bench.bench_config = { iterations = 10; warmup = 2 }

let medium_config: Bench.bench_config = { iterations = 5; warmup = 1 }

let large_config: Bench.bench_config = { iterations = 1; warmup = 0 }

let op_sizes = [
  { label = "1k ops"; count = 1_000; config = small_config };
  { label = "10k ops"; count = 10_000; config = small_config };
]

let make_hashmap = fun ~size -> HashMap.with_capacity ~size

let fill_hashmap = fun map count ->
  for index = 0 to count - 1 do
    ignore (HashMap.insert map ~key:index ~value:index)
  done

let fill_concurrent = fun map count ->
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.insert map ~key:index ~value:index)
  done

let bench_hashmap_create = fun count () ->
  for _ = 1 to count do
    ignore (make_hashmap ~size:0)
  done

let bench_concurrent_create = fun count () ->
  for _ = 1 to count do
    ignore (ConcurrentHashMap.create ())
  done

let bench_hashmap_with_capacity = fun count () ->
  for _ = 1 to count do
    ignore (make_hashmap ~size:1_000)
  done

let bench_concurrent_with_capacity = fun count () ->
  for _ = 1 to count do
    ignore (ConcurrentHashMap.with_capacity ~size:1_000)
  done

let bench_hashmap_from_list = fun count () ->
  let values = List.init ~count ~fn:(fun index -> (index, index)) in
  let map = make_hashmap ~size:count in
  List.for_each values ~fn:(fun (key, value) -> ignore (HashMap.insert map ~key ~value))

let bench_concurrent_from_list = fun count () ->
  let values = List.init ~count ~fn:(fun index -> (index, index)) in
  ignore (ConcurrentHashMap.from_list values)

let bench_hashmap_insert = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count

let bench_concurrent_insert = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count

let bench_hashmap_get = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  for index = 0 to count - 1 do
    ignore (HashMap.get map ~key:index)
  done

let bench_concurrent_get = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.get map ~key:index)
  done

let bench_hashmap_remove = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  for index = 0 to count - 1 do
    ignore (HashMap.remove map ~key:index)
  done

let bench_concurrent_remove = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.remove map ~key:index)
  done

let bench_hashmap_has_key = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  for index = 0 to count - 1 do
    ignore (HashMap.has_key map ~key:index)
  done

let bench_concurrent_has_key = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.has_key map ~key:index)
  done

let bench_hashmap_length = fun count () ->
  let map = make_hashmap ~size:10_000 in
  fill_hashmap map 10_000;
  for _ = 1 to count do
    ignore (HashMap.length map)
  done

let bench_concurrent_length = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:10_000 in
  fill_concurrent map 10_000;
  for _ = 1 to count do
    ignore (ConcurrentHashMap.length map)
  done

let bench_hashmap_clear = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  HashMap.clear map

let bench_concurrent_clear = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ConcurrentHashMap.clear map

let bench_hashmap_keys = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (HashMap.keys map)

let bench_concurrent_keys = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (ConcurrentHashMap.keys map)

let bench_hashmap_values = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (HashMap.values map)

let bench_concurrent_values = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (ConcurrentHashMap.values map)

let bench_hashmap_for_each = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  let total = ref 0 in
  HashMap.for_each map ~fn:(fun key value -> total := !total + key + value)

let bench_concurrent_for_each = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  let total = ref 0 in
  ConcurrentHashMap.for_each map ~fn:(fun key value -> total := !total + key + value)

let bench_hashmap_fold_left = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (HashMap.fold_left map ~init:0 ~fn:(fun acc key value -> acc + key + value))

let bench_concurrent_fold_left = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (ConcurrentHashMap.fold_left map ~init:0 ~fn:(fun acc key value -> acc + key + value))

let bench_hashmap_to_list = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (HashMap.to_list map)

let bench_concurrent_to_list = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (ConcurrentHashMap.to_list map)

let bench_hashmap_entry = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  for index = 0 to count - 1 do
    ignore (HashMap.entry map ~key:index)
  done

let bench_concurrent_entry = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  for index = 0 to count - 1 do
    ignore (ConcurrentHashMap.entry map ~key:index)
  done

let bench_hashmap_iter = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (Iterator.to_list (HashMap.iter map))

let bench_concurrent_iter = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (Iterator.to_list (ConcurrentHashMap.iter map))

let bench_hashmap_mut_iter = fun count () ->
  let map = make_hashmap ~size:count in
  fill_hashmap map count;
  ignore (MutIterator.to_list (HashMap.mut_iter map))

let bench_concurrent_mut_iter = fun count () ->
  let map = ConcurrentHashMap.with_capacity ~size:count in
  fill_concurrent map count;
  ignore (MutIterator.to_list (ConcurrentHashMap.mut_iter map))

let bench_hashmap_compute = fun count () ->
  let map = make_hashmap ~size:1 in
  ignore (HashMap.insert map ~key:0 ~value:0);
  for _ = 1 to count do
    HashMap.compute
      map
      ~key:0
      ~fn:(fun value ->
        let current = Option.unwrap_or value ~default:0 in
        HashMap.Insert (current + 1, ()))
  done

let bench_concurrent_compute = fun count () ->
  let map = ConcurrentHashMap.create () in
  ignore (ConcurrentHashMap.insert map ~key:0 ~value:0);
  for _ = 1 to count do
    ConcurrentHashMap.compute
      map
      ~key:0
      ~fn:(fun value ->
        let current = Option.unwrap_or value ~default:0 in
        ConcurrentHashMap.Insert (current + 1, ()))
  done

let compare_sequential = fun ~config workload hash_map concurrent_hash_map ->
  Bench.compare_with_config
    ~config
    ("Sequential HashMap vs ConcurrentHashMap: " ^ workload)
    [
      Bench.make_case_with_config ~config ("HM: " ^ workload) hash_map;
      Bench.make_case_with_config ~config ("CHM: " ^ workload) concurrent_hash_map;
    ]

let benchmarks_for_size = fun { label; count; config } -> [
  compare_sequential
    ~config
    ("Create " ^ label ^ " empty maps")
    (bench_hashmap_create count)
    (bench_concurrent_create count);
  compare_sequential
    ~config
    ("Create " ^ label ^ " maps with 1k capacity")
    (bench_hashmap_with_capacity count)
    (bench_concurrent_with_capacity count);
  compare_sequential
    ~config
    ("Build from " ^ label)
    (bench_hashmap_from_list count)
    (bench_concurrent_from_list count);
  compare_sequential
    ~config
    ("Insert " ^ label)
    (bench_hashmap_insert count)
    (bench_concurrent_insert count);
  compare_sequential ~config ("Get " ^ label) (bench_hashmap_get count) (bench_concurrent_get count);
  compare_sequential
    ~config
    ("Remove " ^ label)
    (bench_hashmap_remove count)
    (bench_concurrent_remove count);
  compare_sequential
    ~config
    ("Check " ^ label ^ " with has_key")
    (bench_hashmap_has_key count)
    (bench_concurrent_has_key count);
  compare_sequential
    ~config
    ("Read length " ^ label ^ " on 10k items")
    (bench_hashmap_length count)
    (bench_concurrent_length count);
  compare_sequential
    ~config
    ("Clear " ^ label)
    (bench_hashmap_clear count)
    (bench_concurrent_clear count);
  compare_sequential
    ~config
    ("Collect " ^ label ^ " keys")
    (bench_hashmap_keys count)
    (bench_concurrent_keys count);
  compare_sequential
    ~config
    ("Collect " ^ label ^ " values")
    (bench_hashmap_values count)
    (bench_concurrent_values count);
  compare_sequential
    ~config
    ("For_each over " ^ label)
    (bench_hashmap_for_each count)
    (bench_concurrent_for_each count);
  compare_sequential
    ~config
    ("Fold_left over " ^ label)
    (bench_hashmap_fold_left count)
    (bench_concurrent_fold_left count);
  compare_sequential
    ~config
    ("Convert " ^ label ^ " to list")
    (bench_hashmap_to_list count)
    (bench_concurrent_to_list count);
  compare_sequential
    ~config
    ("Read " ^ label ^ " entries")
    (bench_hashmap_entry count)
    (bench_concurrent_entry count);
  compare_sequential
    ~config
    ("Iter over " ^ label)
    (bench_hashmap_iter count)
    (bench_concurrent_iter count);
  compare_sequential
    ~config
    ("Mut_iter over " ^ label)
    (bench_hashmap_mut_iter count)
    (bench_concurrent_mut_iter count);
  compare_sequential
    ~config
    ("Compute same key " ^ label)
    (bench_hashmap_compute count)
    (bench_concurrent_compute count);
]

let benchmarks = List.concat (List.map op_sizes ~fn:benchmarks_for_size)

let main ~args = Bench.Cli.main ~name:"Comparative Sequential HashMap Benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
