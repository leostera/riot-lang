open Std
open Poneglyph

(* Helper: Create a temporary directory for LSM storage *)
let setup_test_dir () =
  let test_dir = "/tmp/poneglyph_bench_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

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

(* ============================= Find Entities Benchmarks ============================= *)

let bench_find_entities_in_1k () =
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
  
  (* Benchmark: find all entities with status=active *)
  let _ = Poneglyph.find_entities graph ~attr ~value:status_value
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_find_entities_in_10k () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:status" in
  let status_value = Fact.String "active" in
  
  (* Create 10k facts, 1k with matching value *)
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let value = if i mod 10 = 0 then status_value else Fact.String "inactive" in
    make_fact ~entity ~attribute:attr ~value
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all entities with status=active *)
  let _ = Poneglyph.find_entities graph ~attr ~value:status_value
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_find_entities_in_100k () =
  (* Use InMemory for large benchmarks *)
  let graph = Poneglyph.create ~config:InMemory () in
  
  let attr = Uri.of_string "@field:status" in
  let status_value = Fact.String "active" in
  
  (* Create 100k facts, 10k with matching value *)
  let facts = List.init 100_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let value = if i mod 10 = 0 then status_value else Fact.String "inactive" in
    make_fact ~entity ~attribute:attr ~value
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all entities with status=active *)
  let _ = Poneglyph.find_entities graph ~attr ~value:status_value
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph

(* ============================= Find By Kind Benchmarks ============================= *)

let bench_find_by_kind_100_entities () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let kind_attr = Uri.of_string "@field:instance_of" in
  let file_kind = Uri.of_string "tusk:kind:file" in
  let module_kind = Uri.of_string "tusk:kind:module" in
  
  (* Create 100 file entities and 100 module entities *)
  let facts = List.init 200 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let kind = if i < 100 then file_kind else module_kind in
    make_fact ~entity ~attribute:kind_attr ~value:(Fact.Uri kind)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all file entities *)
  let _ = Poneglyph.find_by_kind graph ~kind:file_kind
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_find_by_kind_1k_entities () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let kind_attr = Uri.of_string "@field:instance_of" in
  let file_kind = Uri.of_string "tusk:kind:file" in
  let module_kind = Uri.of_string "tusk:kind:module" in
  
  (* Create 1k file entities and 1k module entities *)
  let facts = List.init 2_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let kind = if i < 1_000 then file_kind else module_kind in
    make_fact ~entity ~attribute:kind_attr ~value:(Fact.Uri kind)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all file entities *)
  let _ = Poneglyph.find_by_kind graph ~kind:file_kind
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_find_by_kind_10k_entities () =
  (* Use InMemory for large benchmarks *)
  let graph = Poneglyph.create ~config:InMemory () in
  
  let kind_attr = Uri.of_string "@field:instance_of" in
  let file_kind = Uri.of_string "tusk:kind:file" in
  let module_kind = Uri.of_string "tusk:kind:module" in
  
  (* Create 10k file entities and 10k module entities *)
  let facts = List.init 20_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let kind = if i < 10_000 then file_kind else module_kind in
    make_fact ~entity ~attribute:kind_attr ~value:(Fact.Uri kind)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all file entities *)
  let _ = Poneglyph.find_by_kind graph ~kind:file_kind
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph

(* ============================= Transitive Traversal Benchmarks ============================= *)

let bench_transitive_depth_5_100_nodes () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let depends_on = Uri.of_string "@field:depends_on" in
  
  (* Create a chain: 0 -> 1 -> 2 -> ... -> 99 *)
  let facts = List.init 99 (fun i ->
    let entity = Uri.of_string ("module:" ^ string_of_int i) in
    let target = Uri.of_string ("module:" ^ string_of_int (i + 1)) in
    make_fact ~entity ~attribute:depends_on ~value:(Fact.Uri target)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: transitive traversal with depth limit *)
  let start = Uri.of_string "module:0" in
  let _ = Poneglyph.transitive graph ~start ~edge:depends_on ~max_depth:(Some 5)
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_transitive_depth_10_1k_nodes () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let depends_on = Uri.of_string "@field:depends_on" in
  
  (* Create a chain: 0 -> 1 -> 2 -> ... -> 999 *)
  let facts = List.init 999 (fun i ->
    let entity = Uri.of_string ("module:" ^ string_of_int i) in
    let target = Uri.of_string ("module:" ^ string_of_int (i + 1)) in
    make_fact ~entity ~attribute:depends_on ~value:(Fact.Uri target)
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: transitive traversal with depth limit *)
  let start = Uri.of_string "module:0" in
  let _ = Poneglyph.transitive graph ~start ~edge:depends_on ~max_depth:(Some 10)
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_transitive_dag_100_nodes () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let depends_on = Uri.of_string "@field:depends_on" in
  
  (* Create a DAG where each node depends on 2 previous nodes *)
  let facts = List.init 98 (fun i ->
    let entity = Uri.of_string ("module:" ^ string_of_int (i + 2)) in
    [
      make_fact ~entity ~attribute:depends_on 
        ~value:(Fact.Uri (Uri.of_string ("module:" ^ string_of_int i)));
      make_fact ~entity ~attribute:depends_on 
        ~value:(Fact.Uri (Uri.of_string ("module:" ^ string_of_int (i + 1))));
    ]
  ) |> List.flatten in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: transitive traversal on DAG *)
  let start = Uri.of_string "module:99" in
  let _ = Poneglyph.transitive graph ~start ~edge:depends_on ~max_depth:None
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Find By Source Benchmarks ============================= *)

let bench_find_by_source_1k_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  let source1 = Uri.of_string "build:001" in
  let source2 = Uri.of_string "build:002" in
  
  (* Create 1k facts, half from each source *)
  let facts = List.init 1_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let source_uri = if i mod 2 = 0 then source1 else source2 in
    let fact_uri = Uri.of_string ("fact:" ^ string_of_int i) in
    let stated_at = Datetime.now () in
    let tx_id = UUID.v7_monotonic () in
    {
      Fact.fact_uri;
      source_uri;
      entity;
      attribute = attr;
      value = Fact.Int i;
      stated_at;
      tx_id;
      retracted = false;
    }
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all entities from source1 *)
  let _ = Poneglyph.find_by_source graph ~source:source1
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

let bench_find_by_source_10k_facts () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let attr = Uri.of_string "@field:value" in
  let source1 = Uri.of_string "build:001" in
  let source2 = Uri.of_string "build:002" in
  
  (* Create 10k facts, half from each source *)
  let facts = List.init 10_000 (fun i ->
    let entity = Uri.of_string ("entity:" ^ string_of_int i) in
    let source_uri = if i mod 2 = 0 then source1 else source2 in
    let fact_uri = Uri.of_string ("fact:" ^ string_of_int i) in
    let stated_at = Datetime.now () in
    let tx_id = UUID.v7_monotonic () in
    {
      Fact.fact_uri;
      source_uri;
      entity;
      attribute = attr;
      value = Fact.Int i;
      stated_at;
      tx_id;
      retracted = false;
    }
  ) in
  
  let _tx_id = Poneglyph.state graph facts in
  
  (* Benchmark: find all entities from source1 *)
  let _ = Poneglyph.find_by_source graph ~source:source1
    |> Iter.MutIterator.to_list in
  
  Poneglyph.close graph;
  cleanup_test_dir dir

(* ============================= Benchmark List ============================= *)

let benchmarks =
  Bench.[
    (* Find entities - reverse lookup by attribute/value *)
    with_config ~config:{ iterations = 20; warmup = 2 } "find_entities: 1k facts"
      bench_find_entities_in_1k;
    with_config ~config:{ iterations = 10; warmup = 2 } "find_entities: 10k facts"
      bench_find_entities_in_10k;
    with_config ~config:{ iterations = 3; warmup = 1 } "find_entities: 100k facts"
      bench_find_entities_in_100k;
    
    (* Find by kind - type-based queries *)
    case "find_by_kind: 100 entities" bench_find_by_kind_100_entities;
    with_config ~config:{ iterations = 20; warmup = 2 } "find_by_kind: 1k entities"
      bench_find_by_kind_1k_entities;
    with_config ~config:{ iterations = 5; warmup = 1 } "find_by_kind: 10k entities"
      bench_find_by_kind_10k_entities;
    
    (* Transitive traversal - graph navigation *)
    case "transitive: depth 5, 100 nodes (chain)" bench_transitive_depth_5_100_nodes;
    with_config ~config:{ iterations = 10; warmup = 2 } "transitive: depth 10, 1k nodes (chain)"
      bench_transitive_depth_10_1k_nodes;
    with_config ~config:{ iterations = 10; warmup = 2 } "transitive: unbounded, 100 nodes (DAG)"
      bench_transitive_dag_100_nodes;
    
    (* Find by source - provenance queries *)
    with_config ~config:{ iterations = 20; warmup = 2 } "find_by_source: 1k facts"
      bench_find_by_source_1k_facts;
    with_config ~config:{ iterations = 10; warmup = 2 } "find_by_source: 10k facts"
      bench_find_by_source_10k_facts;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      let config =
        Bench.Runner.{
          reporter = (module Bench.Reporter.Default);
          suite_info = { name = "Poneglyph Query Operations" };
        }
      in
      let _summary = Bench.Runner.run_benchmarks ~config benchmarks in
      Ok ())
    ~args:Env.args ()
