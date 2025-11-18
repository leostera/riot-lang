open Std
open Std.UUID
open Poneglyph
open Poneglyph.Storage.Lsm

let () = Random.init 42

let setup_test_dir () =
  let test_dir = "/tmp/multi_store_test_" ^ string_of_int (Random.int 1000000) in
  ignore (Fs.create_dir_all (Path.v test_dir));
  test_dir

let cleanup_test_dir dir = ignore (Fs.remove_dir_all (Path.v dir))

(* Helper to create test facts *)
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

(* ============================= Unit Tests ============================= *)

let test_create_multi_store () =
  let dir = setup_test_dir () in
  
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_create_multi_store - " ^ err);
      exit 1
  | Ok store -> (
      match Multi_store.close store with
      | Error err ->
          cleanup_test_dir dir;
          println ("FAIL: test_create_multi_store - close: " ^ err);
          exit 1
      | Ok () ->
          cleanup_test_dir dir;
          println "PASS: test_create_multi_store")

let test_state_single_fact () =
  let dir = setup_test_dir () in
  
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_state_single_fact - " ^ err);
      exit 1
  | Ok store -> (
      let entity = Uri.of_string "person:alice" in
      let attribute = Uri.of_string "@field:name" in
      let value = Fact.Uri (Uri.of_string "value:alice") in
      
      let fact = make_fact ~entity ~attribute ~value in
      
      match Multi_store.state store [fact] with
      | Error err ->
          ignore (Multi_store.close store);
          cleanup_test_dir dir;
          println ("FAIL: test_state_single_fact - state: " ^ err);
          exit 1
      | Ok count ->
          if count != 1 then (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println "FAIL: test_state_single_fact - wrong count";
            exit 1)
          else (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println "PASS: test_state_single_fact"))

let test_state_multiple_facts () =
  let dir = setup_test_dir () in
  
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_state_multiple_facts - " ^ err);
      exit 1
  | Ok store -> (
      let alice = Uri.of_string "person:alice" in
      let bob = Uri.of_string "person:bob" in
      let name_attr = Uri.of_string "@field:name" in
      let age_attr = Uri.of_string "@field:age" in
      
      let facts = [
        make_fact ~entity:alice ~attribute:name_attr ~value:(Fact.Uri (Uri.of_string "value:alice"));
        make_fact ~entity:alice ~attribute:age_attr ~value:(Fact.Int 30);
        make_fact ~entity:bob ~attribute:name_attr ~value:(Fact.Uri (Uri.of_string "value:bob"));
      ] in
      
      match Multi_store.state store facts with
      | Error err ->
          ignore (Multi_store.close store);
          cleanup_test_dir dir;
          println ("FAIL: test_state_multiple_facts - state: " ^ err);
          exit 1
      | Ok count ->
          if count != 3 then (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println ("FAIL: test_state_multiple_facts - wrong count: " ^ string_of_int count);
            exit 1)
          else (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println "PASS: test_state_multiple_facts"))

let test_close_and_reopen () =
  let dir = setup_test_dir () in
  
  (* Create and write *)
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_close_and_reopen - create: " ^ err);
      exit 1
  | Ok store -> (
      let entity = Uri.of_string "person:alice" in
      let attribute = Uri.of_string "@field:name" in
      let value = Fact.Uri (Uri.of_string "value:alice") in
      let fact = make_fact ~entity ~attribute ~value in
      
      match Multi_store.state store [fact] with
      | Error err ->
          ignore (Multi_store.close store);
          cleanup_test_dir dir;
          println ("FAIL: test_close_and_reopen - state: " ^ err);
          exit 1
      | Ok _ -> (
          match Multi_store.close store with
          | Error err ->
              cleanup_test_dir dir;
              println ("FAIL: test_close_and_reopen - close: " ^ err);
              exit 1
          | Ok () -> (
              (* Reopen *)
              match Multi_store.create ~data_dir:dir with
              | Error err ->
                  cleanup_test_dir dir;
                  println ("FAIL: test_close_and_reopen - reopen: " ^ err);
                  exit 1
              | Ok store2 -> (
                  (* Just verify it opens successfully *)
                  match Multi_store.close store2 with
                  | Error err ->
                      cleanup_test_dir dir;
                      println ("FAIL: test_close_and_reopen - close2: " ^ err);
                      exit 1
                  | Ok () ->
                      cleanup_test_dir dir;
                      println "PASS: test_close_and_reopen"))))

let test_atomic_write_all_indices () =
  let dir = setup_test_dir () in
  
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_atomic_write_all_indices - " ^ err);
      exit 1
  | Ok store -> (
      (* Write multiple facts atomically *)
      let entity = Uri.of_string "person:alice" in
      let facts = [
        make_fact ~entity ~attribute:(Uri.of_string "@field:name") 
          ~value:(Fact.Uri (Uri.of_string "value:alice"));
        make_fact ~entity ~attribute:(Uri.of_string "@field:age") 
          ~value:(Fact.Int 30);
        make_fact ~entity ~attribute:(Uri.of_string "@field:active") 
          ~value:(Fact.Bool true);
      ] in
      
      match Multi_store.state store facts with
      | Error err ->
          ignore (Multi_store.close store);
          cleanup_test_dir dir;
          println ("FAIL: test_atomic_write_all_indices - state: " ^ err);
          exit 1
      | Ok count ->
          if count != 3 then (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println "FAIL: test_atomic_write_all_indices - wrong count";
            exit 1)
          else (
            (* Verify all 4 index directories exist *)
            let check_dir name =
              let path = Path.v (dir ^ "/" ^ name) in
              match Fs.exists path with
              | Ok true -> true
              | _ -> false
            in
            
            if not (check_dir "eavt") then (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "FAIL: test_atomic_write_all_indices - eavt dir missing";
              exit 1)
            else if not (check_dir "avet") then (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "FAIL: test_atomic_write_all_indices - avet dir missing";
              exit 1)
            else if not (check_dir "fact") then (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "FAIL: test_atomic_write_all_indices - fact dir missing";
              exit 1)
            else if not (check_dir "source") then (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "FAIL: test_atomic_write_all_indices - source dir missing";
              exit 1)
            else (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "PASS: test_atomic_write_all_indices")))

(* ============================= Query Tests ============================= *)

let test_query_entity_facts_full () =
  let dir = setup_test_dir () in
  
  match Multi_store.create ~data_dir:dir with
  | Error err ->
      cleanup_test_dir dir;
      println ("FAIL: test_query_entity_facts_full - " ^ err);
      exit 1
  | Ok store -> (
      let alice = Uri.of_string "person:alice" in
      let bob = Uri.of_string "person:bob" in
      let name_attr = Uri.of_string "@field:name" in
      let age_attr = Uri.of_string "@field:age" in
      
      (* Write facts for Alice and Bob *)
      let facts = [
        make_fact ~entity:alice ~attribute:name_attr ~value:(Fact.Uri (Uri.of_string "value:alice"));
        make_fact ~entity:alice ~attribute:age_attr ~value:(Fact.Int 30);
        make_fact ~entity:bob ~attribute:name_attr ~value:(Fact.Uri (Uri.of_string "value:bob"));
      ] in
      
      match Multi_store.state store facts with
      | Error err ->
          ignore (Multi_store.close store);
          cleanup_test_dir dir;
          println ("FAIL: test_query_entity_facts_full - state: " ^ err);
          exit 1
       | Ok _ -> (
          (* Query facts for Alice - now works with full scan_prefix! *)
          let alice_facts = Multi_store.get_entity_facts store ~entity:alice 
            |> Iter.MutIterator.to_list in
          
          if List.length alice_facts != 2 then (
            ignore (Multi_store.close store);
            cleanup_test_dir dir;
            println ("FAIL: test_query_entity_facts_full - expected 2 facts for Alice, got " ^ 
                    string_of_int (List.length alice_facts));
            exit 1)
          else (
            (* Query facts for Bob *)
            let bob_facts = Multi_store.get_entity_facts store ~entity:bob 
              |> Iter.MutIterator.to_list in
            
            if List.length bob_facts != 1 then (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println ("FAIL: test_query_entity_facts_full - expected 1 fact for Bob, got " ^
                      string_of_int (List.length bob_facts));
              exit 1)
            else (
              ignore (Multi_store.close store);
              cleanup_test_dir dir;
              println "PASS: test_query_entity_facts_full"))))

let test_find_entities_by_attr_value () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir 
    |> Result.expect ~msg:"test_find_entities_by_attr_value: create store" in
  
  let alice = Uri.of_string "person:alice" in
  let bob = Uri.of_string "person:bob" in
  let charlie = Uri.of_string "person:charlie" in
  let name_attr = Uri.of_string "@field:name" in
  let age_attr = Uri.of_string "@field:age" in
  
  (* Create facts: Alice and Charlie have same name, Bob has different name *)
  let alice_name = Uri.of_string "name:alice" in
  let bob_name = Uri.of_string "name:bob" in
  
  let facts = [
    make_fact ~entity:alice ~attribute:name_attr ~value:(Fact.Uri alice_name);
    make_fact ~entity:alice ~attribute:age_attr ~value:(Fact.Int 30);
    make_fact ~entity:bob ~attribute:name_attr ~value:(Fact.Uri bob_name);
    make_fact ~entity:bob ~attribute:age_attr ~value:(Fact.Int 25);
    make_fact ~entity:charlie ~attribute:name_attr ~value:(Fact.Uri alice_name);
    make_fact ~entity:charlie ~attribute:age_attr ~value:(Fact.Int 28);
  ] in
  
  let _ = Multi_store.state store facts 
    |> Result.expect ~msg:"test_find_entities_by_attr_value: state facts" in
  
  (* Query: Find all people with name alice_name - should find alice and charlie *)
  let alice_entities = Multi_store.find_entities_by_attr_value store
    ~attribute:name_attr
    ~value:(Fact.Uri alice_name)
    |> Iter.MutIterator.to_list in
  
  if List.length alice_entities != 2 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_find_entities_by_attr_value - expected 2 entities with alice_name, got " ^
            string_of_int (List.length alice_entities));
    exit 1);
  
  (* Check that we got alice and charlie (order doesn't matter) *)
  let has_alice = List.exists (fun e -> Uri.equal e alice) alice_entities in
  let has_charlie = List.exists (fun e -> Uri.equal e charlie) alice_entities in
  
  if not has_alice || not has_charlie then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println "FAIL: test_find_entities_by_attr_value - didn't find correct entities";
    exit 1);
  
  (* Query: Find all people aged 30 - should find only alice *)
  let age_30_entities = Multi_store.find_entities_by_attr_value store
    ~attribute:age_attr
    ~value:(Fact.Int 30)
    |> Iter.MutIterator.to_list in
  
  if List.length age_30_entities != 1 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_find_entities_by_attr_value - expected 1 entity aged 30, got " ^
            string_of_int (List.length age_30_entities));
    exit 1);
  
  if not (Uri.equal (List.hd age_30_entities) alice) then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println "FAIL: test_find_entities_by_attr_value - wrong entity aged 30";
    exit 1);
  
  ignore (Multi_store.close store);
  cleanup_test_dir dir;
  println "PASS: test_find_entities_by_attr_value"

let test_retract_facts () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir 
    |> Result.expect ~msg:"test_retract_facts: create store" in
  
  let alice = Uri.of_string "person:alice" in
  let name_attr = Uri.of_string "@field:name" in
  let age_attr = Uri.of_string "@field:age" in
  let alice_name = Uri.of_string "name:alice" in
  
  (* State two facts for Alice *)
  let name_fact = make_fact ~entity:alice ~attribute:name_attr ~value:(Fact.Uri alice_name) in
  let age_fact = make_fact ~entity:alice ~attribute:age_attr ~value:(Fact.Int 30) in
  let facts = [name_fact; age_fact] in
  
  let _ = Multi_store.state store facts 
    |> Result.expect ~msg:"test_retract_facts: state facts" in
  
  (* Verify we have 2 facts *)
  let alice_facts = Multi_store.get_entity_facts store ~entity:alice 
    |> Iter.MutIterator.to_list in
  if List.length alice_facts != 2 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_retract_facts - expected 2 facts before retraction, got " ^
            string_of_int (List.length alice_facts));
    exit 1);
  
  (* Retract the age fact *)
  let _ = Multi_store.retract store [age_fact]
    |> Result.expect ~msg:"test_retract_facts: retract age fact" in
  
  (* Verify we now have only 1 fact (name) *)
  let alice_facts_after = Multi_store.get_entity_facts store ~entity:alice 
    |> Iter.MutIterator.to_list in
  if List.length alice_facts_after != 1 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_retract_facts - expected 1 fact after retraction, got " ^
            string_of_int (List.length alice_facts_after));
    exit 1);
  
  (* Verify the remaining fact is the name fact *)
  let remaining_fact = List.hd alice_facts_after in
  if not (Uri.equal remaining_fact.Fact.attribute name_attr) then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println "FAIL: test_retract_facts - wrong fact remained after retraction";
    exit 1);
  
  (* Verify AVET query also filters retracted facts *)
  let age_30_entities = Multi_store.find_entities_by_attr_value store
    ~attribute:age_attr
    ~value:(Fact.Int 30)
    |> Iter.MutIterator.to_list in
  
  if List.length age_30_entities != 0 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_retract_facts - AVET query should return 0 entities, got " ^
            string_of_int (List.length age_30_entities));
    exit 1);
  
  ignore (Multi_store.close store);
  cleanup_test_dir dir;
  println "PASS: test_retract_facts"

(* ============================= String Value Tests ============================= *)

let test_string_fact_roundtrip () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"test_string_fact_roundtrip: create store" in
  
  let entity = Uri.of_string "test:entity" in
  let attr = Uri.of_string "@field:message" in
  
  (* Create fact with string value *)
  let fact = make_fact ~entity ~attribute:attr ~value:(Fact.String "hello world") in
  
  let _ = Multi_store.state store [fact]
    |> Result.expect ~msg:"test_string_fact_roundtrip: state fact" in
  
  (* Query back *)
  let results = Multi_store.get_entity_facts store ~entity
    |> Iter.MutIterator.to_list in
  
  if List.length results != 1 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_string_fact_roundtrip - expected 1 fact, got " ^
            string_of_int (List.length results));
    exit 1);
  
  let retrieved = List.hd results in
  (match retrieved.Fact.value with
   | Fact.String s when s = "hello world" ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "PASS: test_string_fact_roundtrip"
   | Fact.String s ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println ("FAIL: test_string_fact_roundtrip - wrong string value: " ^ s);
       exit 1
   | _ ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "FAIL: test_string_fact_roundtrip - wrong value type";
       exit 1)

let test_multiple_string_values () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"test_multiple_string_values: create store" in
  
  let entity = Uri.of_string "test:entity" in
  let attr1 = Uri.of_string "@field:name" in
  let attr2 = Uri.of_string "@field:status" in
  
  let fact1 = make_fact ~entity ~attribute:attr1 ~value:(Fact.String "Alice") in
  let fact2 = make_fact ~entity ~attribute:attr2 ~value:(Fact.String "active") in
  
  let _ = Multi_store.state store [fact1; fact2]
    |> Result.expect ~msg:"test_multiple_string_values: state facts" in
  
  let results = Multi_store.get_entity_facts store ~entity
    |> Iter.MutIterator.to_list in
  
  if List.length results != 2 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_multiple_string_values - expected 2 facts, got " ^
            string_of_int (List.length results));
    exit 1);
  
  ignore (Multi_store.close store);
  cleanup_test_dir dir;
  println "PASS: test_multiple_string_values"

let test_empty_string () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"test_empty_string: create store" in
  
  let entity = Uri.of_string "test:entity" in
  let attr = Uri.of_string "@field:empty" in
  
  let fact = make_fact ~entity ~attribute:attr ~value:(Fact.String "") in
  
  let _ = Multi_store.state store [fact]
    |> Result.expect ~msg:"test_empty_string: state fact" in
  
  let results = Multi_store.get_entity_facts store ~entity
    |> Iter.MutIterator.to_list in
  
  if List.length results != 1 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_empty_string - expected 1 fact, got " ^
            string_of_int (List.length results));
    exit 1);
  
  let retrieved = List.hd results in
  (match retrieved.Fact.value with
   | Fact.String "" ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "PASS: test_empty_string"
   | _ ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "FAIL: test_empty_string - wrong value";
       exit 1)

let test_long_string () =
  let dir = setup_test_dir () in
  let store = Multi_store.create ~data_dir:dir
    |> Result.expect ~msg:"test_long_string: create store" in
  
  let entity = Uri.of_string "test:entity" in
  let attr = Uri.of_string "@field:data" in
  
  (* Create a string > 1KB *)
  let long_str = String.make 2000 'x' in
  let fact = make_fact ~entity ~attribute:attr ~value:(Fact.String long_str) in
  
  let _ = Multi_store.state store [fact]
    |> Result.expect ~msg:"test_long_string: state fact" in
  
  let results = Multi_store.get_entity_facts store ~entity
    |> Iter.MutIterator.to_list in
  
  if List.length results != 1 then (
    ignore (Multi_store.close store);
    cleanup_test_dir dir;
    println ("FAIL: test_long_string - expected 1 fact, got " ^
            string_of_int (List.length results));
    exit 1);
  
  let retrieved = List.hd results in
  (match retrieved.Fact.value with
   | Fact.String s when s = long_str && String.length s = 2000 ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "PASS: test_long_string"
   | Fact.String s ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println ("FAIL: test_long_string - wrong length: " ^ string_of_int (String.length s));
       exit 1
   | _ ->
       ignore (Multi_store.close store);
       cleanup_test_dir dir;
       println "FAIL: test_long_string - wrong value type";
       exit 1)

(* ============================= Main ============================= *)

let () =
  println "\n=== Multi-Index LSM Store Tests ===\n";
  test_create_multi_store ();
  test_state_single_fact ();
  test_state_multiple_facts ();
  test_close_and_reopen ();
  test_atomic_write_all_indices ();
  
  println "\n=== Multi-Index Query Tests ===\n";
  test_query_entity_facts_full ();
  test_find_entities_by_attr_value ();
  test_retract_facts ();
  
  println "\n=== String Value Tests ===\n";
  test_string_fact_roundtrip ();
  test_multiple_string_values ();
  test_empty_string ();
  test_long_string ();
  
  println "\n=== All Multi-Index Store Tests Passed! ===\n"
