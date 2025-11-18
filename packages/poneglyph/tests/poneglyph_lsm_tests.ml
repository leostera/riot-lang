open Std
open Poneglyph

let () = Random.init 42

let setup_test_dir () =
  let test_dir = "/tmp/poneglyph_lsm_test_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

let make_fact ~entity ~attribute ~value =
  let fact_uri = Uri.of_string ("fact:" ^ string_of_int (Random.int 1000000)) in
  let source_uri = Uri.of_string "test:source" in
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

(* ============================= Tests ============================= *)

let test_create_lsm () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  Poneglyph.close graph;
  cleanup_test_dir dir;
  println "PASS: test_create_lsm"

let test_state_and_query () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  (* Create and state facts *)
  let alice = Uri.of_string "person:alice" in
  let name_attr = Uri.of_string "@field:name" in
  let age_attr = Uri.of_string "@field:age" in
  let alice_name = Uri.of_string "name:Alice" in
  
  let name_fact = make_fact ~entity:alice ~attribute:name_attr 
    ~value:(Fact.Uri alice_name) in
  let age_fact = make_fact ~entity:alice ~attribute:age_attr 
    ~value:(Fact.Int 30) in
  
  let _tx_id = Poneglyph.state graph [name_fact; age_fact] in
  
  (* Query facts *)
  let facts = Poneglyph.get_current_facts graph ~entity:alice 
    |> Iter.MutIterator.to_list in
  if List.length facts != 2 then (
    Poneglyph.close graph;
    cleanup_test_dir dir;
    println ("FAIL: test_state_and_query - expected 2 facts, got " ^ string_of_int (List.length facts));
    exit 1
  );
  
  (* Query specific attribute *)
  match Poneglyph.get graph ~entity:alice ~attr:name_attr with
  | Some (Fact.Uri u) when Uri.equal u alice_name -> ()
  | _ -> (
      Poneglyph.close graph;
      cleanup_test_dir dir;
      println "FAIL: test_state_and_query - expected name to be alice_name URI";
      exit 1
  );
  
  Poneglyph.close graph;
  cleanup_test_dir dir;
  println "PASS: test_state_and_query"

let test_find_entities () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  (* Create entities with same attribute value *)
  let alice = Uri.of_string "person:alice" in
  let bob = Uri.of_string "person:bob" in
  let age_attr = Uri.of_string "@field:age" in
  
  let alice_fact = make_fact ~entity:alice ~attribute:age_attr ~value:(Fact.Int 30) in
  let bob_fact = make_fact ~entity:bob ~attribute:age_attr ~value:(Fact.Int 30) in
  
  let _tx_id = Poneglyph.state graph [alice_fact; bob_fact] in
  
  (* Find entities by attribute value *)
  let entities = Poneglyph.find_entities graph ~attr:age_attr ~value:(Fact.Int 30) 
    |> Iter.MutIterator.to_list in
  if List.length entities != 2 then (
    Poneglyph.close graph;
    cleanup_test_dir dir;
    println ("FAIL: test_find_entities - expected 2 entities, got " ^ string_of_int (List.length entities));
    exit 1
  );
  
  Poneglyph.close graph;
  cleanup_test_dir dir;
  println "PASS: test_find_entities"

let test_exists_and_get_kind () =
  let dir = setup_test_dir () in
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  
  let module_a = Uri.of_string "module:A" in
  let instance_of = Uri.of_string "@field:instance_of" in
  let module_kind = Uri.of_string "@kind:module" in
  
  let fact = make_fact ~entity:module_a ~attribute:instance_of ~value:(Fact.Uri module_kind) in
  let _tx_id = Poneglyph.state graph [fact] in
  
  (* Check existence *)
  if not (Poneglyph.exists graph module_a) then (
    Poneglyph.close graph;
    cleanup_test_dir dir;
    println "FAIL: test_exists_and_get_kind - entity should exist";
    exit 1
  );
  
  (* Get kind *)
  match Poneglyph.get_kind graph module_a with
  | Some kind when Uri.equal kind module_kind -> ()
  | _ -> (
      Poneglyph.close graph;
      cleanup_test_dir dir;
      println "FAIL: test_exists_and_get_kind - wrong kind";
      exit 1
  );
  
  Poneglyph.close graph;
  cleanup_test_dir dir;
  println "PASS: test_exists_and_get_kind"

let test_close_and_reopen () =
  let dir = setup_test_dir () in
  
  (* Create graph, write data, close *)
  let graph = Poneglyph.create ~config:(Lsm dir) () in
  let alice = Uri.of_string "person:alice" in
  let name_attr = Uri.of_string "@field:name" in
  let alice_name = Uri.of_string "name:Alice" in
  let fact = make_fact ~entity:alice ~attribute:name_attr ~value:(Fact.Uri alice_name) in
  let _tx_id = Poneglyph.state graph [fact] in
  Poneglyph.close graph;
  
  (* Reopen and query *)
  let graph2 = Poneglyph.create ~config:(Lsm dir) () in
  let facts = Poneglyph.get_current_facts graph2 ~entity:alice 
    |> Iter.MutIterator.to_list in
  if List.length facts != 1 then (
    Poneglyph.close graph2;
    cleanup_test_dir dir;
    println ("FAIL: test_close_and_reopen - expected 1 fact after reopen, got " ^ string_of_int (List.length facts));
    exit 1
  );
  
  Poneglyph.close graph2;
  cleanup_test_dir dir;
  println "PASS: test_close_and_reopen"

(* ============================= Main ============================= *)

let () =
  println "\n=== Poneglyph LSM Integration Tests ===\n";
  test_create_lsm ();
  test_state_and_query ();
  test_find_entities ();
  test_exists_and_get_kind ();
  test_close_and_reopen ();
  println "\n=== All Poneglyph LSM Tests Passed! ===\n"
