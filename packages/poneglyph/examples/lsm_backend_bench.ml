open Std
open Poneglyph

(* Poneglyph LSM Backend Benchmarks - Full stack performance *)

(* Helper: Create a temporary directory for LSM storage *)
let setup_test_dir () =
  let rand = string_of_int (Random.int 1000000) in
  let test_dir = "/tmp/poneglyph_lsm_bench_" ^ rand in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = 
  ignore (Fs.remove_dir_all (Path.v dir))

(* Helper: Make a fact with random ID *)
let make_fact ~entity ~attribute ~value =
  let fact_uri = Uri.of_string ("fact:" ^ string_of_int (Random.int 1000000)) in
  let source_uri = Uri.of_string "bench:lsm" in
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

(* ============================= LSM State Benchmarks ============================= *)

let bench_lsm_state_100 () =
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

let bench_lsm_state_1k () =
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

let bench_lsm_state_10k () =
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

(* ============================= LSM Get Benchmarks ============================= *)

let bench_lsm_get_from_100 () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 100 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: single get operation *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:50") ~attr in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_lsm_get_from_1k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 1_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: single get operation *)
  let _ = Poneglyph.get graph ~entity:(Uri.of_string "entity:500") ~attr in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_lsm_get_from_10k () =
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

(* ============================= LSM Query Benchmarks ============================= *)

let bench_lsm_get_current_facts_10 () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  
  let facts = List.init 10 (fun i ->
    let attr = Uri.of_string ("@field:attr" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: get all current facts *)
  let _ = Poneglyph.get_current_facts graph ~entity
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_lsm_get_current_facts_100 () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  
  let facts = List.init 100 (fun i ->
    let attr = Uri.of_string ("@field:attr" ^ string_of_int i) in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: get all current facts *)
  let _ = Poneglyph.get_current_facts graph ~entity
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_lsm_find_entities_1k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:status" in
  let status_value = Fact.String "active" in
  
  (* Create 1k facts, 100 with matching value *)
  let facts = List.init 1_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let value = if i mod 10 = 0 then status_value else Fact.String "inactive" in
    make_fact ~entity ~attribute:attr ~value
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find entities by value *)
  let _ = Poneglyph.find_entities graph ~attr ~value:status_value
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= LSM Flush & Close Benchmarks ============================= *)

let bench_lsm_flush_100 () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let entity = Uri.of_string "entity:test" in
  let attr = Uri.of_string "@field:value" in
  
  let facts = List.init 100 (fun i ->
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: manual flush *)
  Poneglyph.flush graph;
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_lsm_flush_1k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let facts = List.init 1_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int (i / 10)) in
    let attr = Uri.of_string "@field:value" in
    make_fact ~entity ~attribute:attr ~value:(Fact.Int i)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: manual flush *)
  Poneglyph.flush graph;
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Benchmark List ============================= *)

let benchmarks =
  Bench.[
    (* LSM state benchmarks - full stack with WAL, memtable, flush *)
    case "lsm.state: 100 facts" bench_lsm_state_100;
    with_config ~config:{ iterations = 20; warmup = 2 } "lsm.state: 1k facts" 
      bench_lsm_state_1k;
    with_config ~config:{ iterations = 5; warmup = 1 } "lsm.state: 10k facts"
      bench_lsm_state_10k;
    
    (* LSM get benchmarks - EAVT index lookup *)
    case "lsm.get: from 100 facts" bench_lsm_get_from_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "lsm.get: from 1k facts"
      bench_lsm_get_from_1k;
    with_config ~config:{ iterations = 5; warmup = 1 } "lsm.get: from 10k facts"
      bench_lsm_get_from_10k;
    
    (* LSM get_current_facts - EAVT scan *)
    case "lsm.get_current_facts: 10 attrs" bench_lsm_get_current_facts_10;
    with_config ~config:{ iterations = 20; warmup = 2 } "lsm.get_current_facts: 100 attrs"
      bench_lsm_get_current_facts_100;
    
    (* LSM find_entities - AVET reverse lookup *)
    with_config ~config:{ iterations = 10; warmup = 2 } "lsm.find_entities: 1k facts"
      bench_lsm_find_entities_1k;
    
    (* LSM flush benchmarks - memtable -> SSTable *)
    case "lsm.flush: 100 facts" bench_lsm_flush_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "lsm.flush: 1k facts"
      bench_lsm_flush_1k;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let config =
        Bench.Runner.{
          reporter = (module Bench.Reporter.Default);
          suite_info = { name = "Poneglyph LSM Backend (Full Stack)" };
        }
      in
      let _summary = Bench.Runner.run_benchmarks ~config benchmarks in
      Ok ())
    ~args:Env.args ()
