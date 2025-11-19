open Std
open Poneglyph

(* Poneglyph Core Operations Benchmarks - v2 *)

(* Helper: Create a temporary directory for LSM storage *)
let setup_test_dir () =
  let rand = string_of_int (Random.int 1000000) in
  let test_dir = "/tmp/poneglyph_bench_" ^ rand in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = 
  ignore (Fs.remove_dir_all (Path.v dir))

(* Helper: Make a fact with random ID *)
let make_fact ~entity ~attribute ~value =
  let fact_uri = Uri.of_string ("fact:" ^ string_of_int (Random.int 1000000)) in
  let source_uri = Uri.of_string "bench:source" in
  let stated_at = Datetime.now () in
  let tx_id = UUID.v7_monotonic () in
  {
    Fact.fact_uri;
    source_uri;
    entity;
    attribute;
    value;
    stated_at;
    tx_id;
    retracted = false;
  }

(* ============================= State Benchmarks ============================= *)

let bench_state_100_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 100 (fun i ->
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_state_1k_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let facts = List.init 1_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int (i / 10)) in
    let attr = Uri.of_string "@field:value" in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_state_10k_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int (i / 10)) in
    let attr = Uri.of_string "@field:value" in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_state_100k_facts () =
  (* Use InMemory for very large benchmarks to avoid LSM overhead *)
  let graph = Poneglyph.create ~config:InMemory () in
  
  (* Insert in batches of 10k to avoid memory issues *)
  for batch = 0 to 9 do
    let offset = batch * 10_000 in
    let facts = List.init 10_000 (fun i ->
      let idx = offset + i in
      let entity = Uri.of_string ("entity:" ^ string_of_int (idx / 10)) in
      let attr = Uri.of_string ("@field:attr" ^ string_of_int (idx mod 10)) in
      make_fact ~entity ~attribute:attr ~value:(Fact.Int idx)
    ) in
    let _tx_id = Poneglyph.state graph facts in
    ()
  done;
  
  Poneglyph.close graph

let bench_state_1m_facts () =
  (* Use InMemory for very large benchmarks to avoid LSM overhead *)
  let graph = Poneglyph.create ~config:InMemory () in
  
  (* Insert in batches of 10k to avoid memory issues *)
  for batch = 0 to 99 do
    let offset = batch * 10_000 in
    let facts = List.init 10_000 (fun i ->
      let idx = offset + i in
      let entity = Uri.of_string ("entity:" ^ string_of_int (idx / 10)) in
      let attr = Uri.of_string ("@field:attr" ^ string_of_int (idx mod 10)) in
      make_fact ~entity ~attribute:attr ~value:(Fact.Int idx)
    ) in
    let _tx_id = Poneglyph.state graph facts in
    ()
  done;
  
  Poneglyph.close graph

(* ============================= Get Benchmarks ============================= *)

let bench_get_from_100_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 100 (fun i ->
    let ent = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity:ent ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: single get operation *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:50") ~attr in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_get_from_10k_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: single get operation *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:5000") ~attr in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_get_from_100k_facts () =
  (* Use InMemory for very large benchmarks *)
  let graph = Poneglyph.create ~config:InMemory () in
  
  let attr = Uri.of_string "@field:value" in
  
  (* Insert in batches of 10k *)
  for batch = 0 to 9 do
    let offset = batch * 10_000 in
    let facts = List.init 10_000 (fun i ->
      let idx = offset + i in
      let entity = Uri.of_string ("entity:" ^ string_of_int idx) in
      make_fact ~entity ~attribute:attr ~value:(Fact.Int idx)
    ) in
    let _tx_id = Poneglyph.state graph facts in
    ()
  done;
  
  (* Benchmark: single get operation *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:50000") ~attr in
  
  Poneglyph.close graph

let bench_get_missing () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: lookup non-existing entity *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:missing") ~attr in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Get Current Facts Benchmarks ============================= *)

let bench_get_current_facts_10_attrs () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  
  let facts = List.init 10 (fun i ->
    let attr = Uri.of_string ("@field:attr" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: get all current facts for entity *)
  let _ = Poneglyph.get_current_facts graph ~entity
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_get_current_facts_100_attrs () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  
  let facts = List.init 100 (fun i ->
    let attr = Uri.of_string ("@field:attr" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: get all current facts for entity *)
  let _ = Poneglyph.get_current_facts graph ~entity
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_get_current_facts_1k_attrs () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  
  let facts = List.init 1_000 (fun i ->
    let attr = Uri.of_string ("@field:attr" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: get all current facts for entity *)
  let _ = Poneglyph.get_current_facts graph ~entity
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Exists Benchmarks ============================= *)

let bench_exists_from_10k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: check if entity exists *)
  let _ = Poneglyph.exists graph (Uri.of_string "entity:5000") in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_exists_missing () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: check if non-existing entity exists *)
  let _ = Poneglyph.exists graph (Uri.of_string "entity:missing") in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Retract Benchmarks ============================= *)

let bench_retract_from_10k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: retract a fact *)
  let fact_to_retract = List.hd facts in
  Poneglyph.retract graph ~fact_uri:fact_to_retract.fact_uri;
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Benchmark List ============================= *)

let benchmarks =
  Bench.[
    (* State benchmarks - insert facts into fresh LSM store *)
    case "state: 100 facts" bench_state_100_facts;
    with_config ~config:{ iterations = 50; warmup = 5 } "state: 1k facts" 
      bench_state_1k_facts;
    with_config ~config:{ iterations = 10; warmup = 2 } "state: 10k facts"
      bench_state_10k_facts;
    with_config ~config:{ iterations = 3; warmup = 1 } "state: 100k facts"
      bench_state_100k_facts;
    with_config ~config:{ iterations = 2; warmup = 1 } "state: 1M facts"
      bench_state_1m_facts;
    
    (* Get benchmarks - single attribute lookup from different sized DBs *)
    case "get: from 100 facts" bench_get_from_100_facts;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: from 10k facts"
      bench_get_from_10k_facts;
    with_config ~config:{ iterations = 5; warmup = 1 } "get: from 100k facts"
      bench_get_from_100k_facts;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: missing entity"
      bench_get_missing;
    
    (* Get current facts - retrieve all facts for an entity *)
    case "get_current_facts: 10 attrs" bench_get_current_facts_10_attrs;
    with_config ~config:{ iterations = 50; warmup = 5 } "get_current_facts: 100 attrs"
      bench_get_current_facts_100_attrs;
    with_config ~config:{ iterations = 10; warmup = 2 } "get_current_facts: 1k attrs"
      bench_get_current_facts_1k_attrs;
    
    (* Exists benchmarks *)
    with_config ~config:{ iterations = 20; warmup = 2 } "exists: from 10k"
      bench_exists_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "exists: missing"
      bench_exists_missing;
    
    (* Retract benchmarks *)
    with_config ~config:{ iterations = 10; warmup = 2 } "retract: from 10k"
      bench_retract_from_10k;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let config =
        Bench.Runner.{
          reporter = (module Bench.Reporter.Default);
          suite_info = { name = "Poneglyph Core Operations" };
        }
      in
      let _summary = Bench.Runner.run_benchmarks ~config benchmarks in
      Ok ())
    ~args:Env.args ()
