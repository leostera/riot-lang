open Std
open Poneglyph

module Bytes = Std.IO.Bytes

(* Helper: Generate random bytes of given length *)
let random_bytes len =
  let s = String.init len (fun _ -> Char.chr (Random.int 256)) in
  Bytes.of_string s

(* Helper: Generate a 41-byte key (LSM key format) *)
let random_key () = random_bytes 41

(* Helper: Generate random value bytes *)
let random_value size = random_bytes size

(* ============================= Memtable Benchmarks ============================= *)

let bench_memtable_add_100 () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  for _i = 1 to 100 do
    let key = random_key () in
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  done

let bench_memtable_add_1k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  for _i = 1 to 1_000 do
    let key = random_key () in
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  done

let bench_memtable_add_10k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:100_000_000 in
  
  for _i = 1 to 10_000 do
    let key = random_key () in
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  done

let bench_memtable_add_batch_1k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  let entries = List.init 1_000 (fun _i ->
    let key = random_key () in
    let value = random_value 100 in
    (key, value)
  ) in
  
  let _ = Memtable.add_batch mt ~entries in
  ()

let bench_memtable_add_batch_10k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:100_000_000 in
  
  let entries = List.init 10_000 (fun _i ->
    let key = random_key () in
    let value = random_value 100 in
    (key, value)
  ) in
  
  let _ = Memtable.add_batch mt ~entries in
  ()

let bench_memtable_get_from_100 () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  (* Populate memtable *)
  let keys = List.init 100 (fun _i -> random_key ()) in
  List.iter (fun key ->
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  ) keys;
  
  (* Benchmark: single lookup *)
  let search_key = List.nth keys 50 in
  let _ = Memtable.get mt ~key:search_key in
  ()

let bench_memtable_get_from_1k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  (* Populate memtable *)
  let keys = List.init 1_000 (fun _i -> random_key ()) in
  List.iter (fun key ->
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  ) keys;
  
  (* Benchmark: single lookup *)
  let search_key = List.nth keys 500 in
  let _ = Memtable.get mt ~key:search_key in
  ()

let bench_memtable_get_from_10k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:100_000_000 in
  
  (* Populate memtable *)
  let keys = List.init 10_000 (fun _i -> random_key ()) in
  List.iter (fun key ->
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  ) keys;
  
  (* Benchmark: single lookup *)
  let search_key = List.nth keys 5_000 in
  let _ = Memtable.get mt ~key:search_key in
  ()

let bench_memtable_iter_1k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:10_000_000 in
  
  (* Populate memtable *)
  for _i = 1 to 1_000 do
    let key = random_key () in
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  done;
  
  (* Benchmark: full iteration *)
  Memtable.iter mt ~f:(fun ~key:_ ~value:_ -> ())

let bench_memtable_iter_10k () =
  let open Storage.Lsm in
  let mt = Memtable.create ~max_size:100_000_000 in
  
  (* Populate memtable *)
  for _i = 1 to 10_000 do
    let key = random_key () in
    let value = random_value 100 in
    let _ = Memtable.add mt ~key ~value in
    ()
  done;
  
  (* Benchmark: full iteration *)
  Memtable.iter mt ~f:(fun ~key:_ ~value:_ -> ())

(* ============================= Bloom Filter Benchmarks ============================= *)

let bench_bloom_add_100 () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:100 ~bits_per_key:10 in
  
  for _i = 1 to 100 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done

let bench_bloom_add_1k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:1_000 ~bits_per_key:10 in
  
  for _i = 1 to 1_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done

let bench_bloom_add_10k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:10_000 ~bits_per_key:10 in
  
  for _i = 1 to 10_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done

let bench_bloom_add_100k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:100_000 ~bits_per_key:10 in
  
  for _i = 1 to 100_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done

let bench_bloom_lookup_from_1k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:1_000 ~bits_per_key:10 in
  
  (* Populate bloom filter *)
  let keys = List.init 1_000 (fun _i -> random_key ()) in
  List.iter (fun key -> Bloom_filter.add bloom ~key) keys;
  
  (* Benchmark: single lookup *)
  let search_key = List.nth keys 500 in
  let _ = Bloom_filter.might_contain bloom ~key:search_key in
  ()

let bench_bloom_lookup_from_10k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:10_000 ~bits_per_key:10 in
  
  (* Populate bloom filter *)
  let keys = List.init 10_000 (fun _i -> random_key ()) in
  List.iter (fun key -> Bloom_filter.add bloom ~key) keys;
  
  (* Benchmark: single lookup *)
  let search_key = List.nth keys 5_000 in
  let _ = Bloom_filter.might_contain bloom ~key:search_key in
  ()

let bench_bloom_lookup_missing () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:10_000 ~bits_per_key:10 in
  
  (* Populate bloom filter *)
  for _i = 1 to 10_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done;
  
  (* Benchmark: lookup non-existing key *)
  let missing_key = random_key () in
  let _ = Bloom_filter.might_contain bloom ~key:missing_key in
  ()

let bench_bloom_serialize_1k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:1_000 ~bits_per_key:10 in
  
  (* Populate bloom filter *)
  for _i = 1 to 1_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done;
  
  (* Benchmark: serialization *)
  let _ = Bloom_filter.to_bytes bloom in
  ()

let bench_bloom_deserialize_1k () =
  let open Storage.Lsm in
  let bloom = Bloom_filter.create ~num_keys:1_000 ~bits_per_key:10 in
  
  (* Populate bloom filter *)
  for _i = 1 to 1_000 do
    let key = random_key () in
    Bloom_filter.add bloom ~key
  done;
  
  let serialized = Bloom_filter.to_bytes bloom in
  
  (* Benchmark: deserialization *)
  let _ = Bloom_filter.from_bytes serialized in
  ()

(* ============================= Encoding Benchmarks ============================= *)

let bench_encode_value_100 () =
  let open Storage.Lsm in
  
  for i = 1 to 100 do
    let value = Fact.Int i in
    let _ = Encoding.encode_value value in
    ()
  done

let bench_encode_value_1k () =
  let open Storage.Lsm in
  
  for i = 1 to 1_000 do
    let value = Fact.Int i in
    let _ = Encoding.encode_value value in
    ()
  done

let bench_decode_value_100 () =
  let open Storage.Lsm in
  
  let value = Fact.Int 42 in
  let (kind, repr) = Encoding.encode_value value in
  
  (* Benchmark: decode 100 times *)
  for _i = 1 to 100 do
    let _ = Encoding.decode_value kind repr in
    ()
  done

let bench_decode_value_1k () =
  let open Storage.Lsm in
  
  let value = Fact.Int 42 in
  let (kind, repr) = Encoding.encode_value value in
  
  (* Benchmark: decode 1k times *)
  for _i = 1 to 1_000 do
    let _ = Encoding.decode_value kind repr in
    ()
  done

let bench_hash_string_100 () =
  let open Storage.Lsm in
  
  for i = 1 to 100 do
    let s = "test_string_" ^ string_of_int i in
    let _ = Encoding.hash_string s in
    ()
  done

let bench_hash_string_1k () =
  let open Storage.Lsm in
  
  for i = 1 to 1_000 do
    let s = "test_string_" ^ string_of_int i in
    let _ = Encoding.hash_string s in
    ()
  done

(* ============================= Benchmark List ============================= *)

let benchmarks =
  Bench.[
    (* Memtable add - single inserts *)
    case "memtable.add: 100 entries" bench_memtable_add_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "memtable.add: 1k entries"
      bench_memtable_add_1k;
    with_config ~config:{ iterations = 10; warmup = 2 } "memtable.add: 10k entries"
      bench_memtable_add_10k;
    
    (* Memtable batch add *)
    with_config ~config:{ iterations = 50; warmup = 5 } "memtable.add_batch: 1k entries"
      bench_memtable_add_batch_1k;
    with_config ~config:{ iterations = 10; warmup = 2 } "memtable.add_batch: 10k entries"
      bench_memtable_add_batch_10k;
    
    (* Memtable get - binary search *)
    case "memtable.get: from 100" bench_memtable_get_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "memtable.get: from 1k"
      bench_memtable_get_from_1k;
    with_config ~config:{ iterations = 20; warmup = 2 } "memtable.get: from 10k"
      bench_memtable_get_from_10k;
    
    (* Memtable iteration *)
    with_config ~config:{ iterations = 50; warmup = 5 } "memtable.iter: 1k entries"
      bench_memtable_iter_1k;
    with_config ~config:{ iterations = 10; warmup = 2 } "memtable.iter: 10k entries"
      bench_memtable_iter_10k;
    
    (* Bloom filter add *)
    case "bloom.add: 100 keys" bench_bloom_add_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "bloom.add: 1k keys"
      bench_bloom_add_1k;
    with_config ~config:{ iterations = 10; warmup = 2 } "bloom.add: 10k keys"
      bench_bloom_add_10k;
    with_config ~config:{ iterations = 3; warmup = 1 } "bloom.add: 100k keys"
      bench_bloom_add_100k;
    
    (* Bloom filter lookup *)
    with_config ~config:{ iterations = 50; warmup = 5 } "bloom.lookup: from 1k"
      bench_bloom_lookup_from_1k;
    with_config ~config:{ iterations = 20; warmup = 2 } "bloom.lookup: from 10k"
      bench_bloom_lookup_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "bloom.lookup: missing"
      bench_bloom_lookup_missing;
    
    (* Bloom filter serialization *)
    with_config ~config:{ iterations = 50; warmup = 5 } "bloom.serialize: 1k keys"
      bench_bloom_serialize_1k;
    with_config ~config:{ iterations = 50; warmup = 5 } "bloom.deserialize: 1k keys"
      bench_bloom_deserialize_1k;
    
    (* Encoding benchmarks *)
    case "encoding.encode_value: 100x" bench_encode_value_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "encoding.encode_value: 1k×"
      bench_encode_value_1k;
    case "encoding.decode_value: 100x" bench_decode_value_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "encoding.decode_value: 1k×"
      bench_decode_value_1k;
    case "encoding.hash_string: 100x" bench_hash_string_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "encoding.hash_string: 1k×"
      bench_hash_string_1k;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let config =
        Bench.Runner.{
          reporter = (module Bench.Reporter.Default);
          suite_info = { name = "Poneglyph LSM Layer" };
        }
      in
      let _summary = Bench.Runner.run_benchmarks ~config benchmarks in
      Ok ())
    ~args:Env.args ()
