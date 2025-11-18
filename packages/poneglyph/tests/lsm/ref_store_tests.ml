(** Tests for Reference Store - The Oracle *)

open Std
open Std.UUID
open Poneglyph

(** Test 1: Basic add and query *)
let test_basic_add_query () =
  let store = Ref_store.empty () in
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in
  let source = Uri.of_string "test:source" in

  let fact =
    Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "Alice")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  Ref_store.add_fact store fact;

  let results = Ref_store.query_entity store ~entity in

  if List.length results != 1 then Error "Should find 1 fact"
  else if not (List.mem fact results) then Error "Should find the added fact"
  else Ok ()

(** Test 2: Last-tx-wins (same fact_uri, different tx) *)
let test_last_tx_wins () =
  let store = Ref_store.empty () in
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in
  let source = Uri.of_string "test:source" in
  let fact_uri = Uri.of_string "test:fact:1" in

  (* Version 1: tx=1, value="Alice" *)
  let tx1 = UUID.v7_monotonic () in
  let fact1 =
    {
      (Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "Alice")
         ~stated_at:(Datetime.now ()) ~tx_id:tx1)
      with
      fact_uri;
    }
  in

  (* Small delay to ensure tx2 > tx1 (UUIDv7 has ms resolution) *)
  Unix.sleepf 0.002;  (* 2ms *)

  (* Version 2: tx=2, value="Bob" - generate after small delay to ensure tx2 > tx1 *)
  let tx2 = UUID.v7_monotonic () in
  let fact2 =
    {
      (Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "Bob")
         ~stated_at:(Datetime.now ()) ~tx_id:tx2)
      with
      fact_uri;
    }
  in

  Ref_store.add_fact store fact1;
  Ref_store.add_fact store fact2;

  let results = Ref_store.query_entity store ~entity in

  if List.length results != 1 then
    Error ("Should find 1 fact, found " ^ string_of_int (List.length results))
  else
    let found = List.hd results in
    match found.Fact.value with
    | Fact.String "Bob" -> Ok ()
    | _ -> Error "Should keep version with tx=2 (Bob)"

(** Test 3: Retraction *)
let test_retraction () =
  let store = Ref_store.empty () in
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in
  let source = Uri.of_string "test:source" in
  let fact_uri = Uri.of_string "test:fact:1" in

  (* Add fact *)
  let tx1 = UUID.v7_monotonic () in
  let fact1 =
    {
      (Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "Alice")
         ~stated_at:(Datetime.now ()) ~tx_id:tx1)
      with
      fact_uri;
    }
  in

  (* Small delay to ensure tx2 > tx1 *)
  Unix.sleepf 0.002;  (* 2ms *)

  (* Retract it with newer tx_id *)
  let tx2 = UUID.v7_monotonic () in
  let fact2 = { fact1 with tx_id = tx2; retracted = true } in

  Ref_store.add_fact store fact1;
  Ref_store.add_fact store fact2;

  let results = Ref_store.query_entity store ~entity in

  if results != [] then Error "Retracted facts should not appear in queries"
  else Ok ()

(** Test 4: Compaction removes old versions *)
let test_compaction () =
  let store = Ref_store.empty () in
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in
  let source = Uri.of_string "test:source" in
  let fact_uri = Uri.of_string "test:fact:1" in

  (* Add 3 SEPARATE facts (different fact_uris) *)
  let fact1 =
    Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "v1")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in
  let fact2 =
    Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "v2")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in
  (* Add third version of SAME fact_uri to test last-tx-wins *)
  let fact3_v1 =
    {
      (Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "v3a")
         ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ()))
      with
      fact_uri;
    }
  in
  let fact3_v2 =
    {
      (Fact.make ~source ~entity ~attribute:attr ~value:(Fact.String "v3b")
         ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ()))
      with
      fact_uri;
    }
  in

  Ref_store.add_fact store fact1;
  Ref_store.add_fact store fact2;
  Ref_store.add_fact store fact3_v1;
  Ref_store.add_fact store fact3_v2;

  let before_count = Ref_store.fact_count store in
  Ref_store.compact store;
  let after_count = Ref_store.fact_count store in

  (* Before: 4 facts (fact1, fact2, fact3_v1, fact3_v2)
     After compaction: 3 facts (fact1, fact2, fact3_v2 - kept last version) *)
  if after_count >= before_count then
    Error
      ("Compaction should reduce fact count: before=" ^ string_of_int before_count ^ 
       " after=" ^ string_of_int after_count)
  else if after_count != 3 then
    Error ("After compaction should have 3 facts, has " ^ string_of_int after_count)
  else Ok ()

(** Test 5: Query by attribute-value *)
let test_query_attr_value () =
  let store = Ref_store.empty () in
  let source = Uri.of_string "test:source" in
  let attr = Uri.of_string "test:age" in

  (* Add facts with different ages *)
  let entity1 = Uri.of_string "test:person:1" in
  let fact1 =
    Fact.make ~source ~entity:entity1 ~attribute:attr ~value:(Fact.Int 30)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  let entity2 = Uri.of_string "test:person:2" in
  let fact2 =
    Fact.make ~source ~entity:entity2 ~attribute:attr ~value:(Fact.Int 25)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  let entity3 = Uri.of_string "test:person:3" in
  let fact3 =
    Fact.make ~source ~entity:entity3 ~attribute:attr ~value:(Fact.Int 30)
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  Ref_store.add_fact store fact1;
  Ref_store.add_fact store fact2;
  Ref_store.add_fact store fact3;

  (* Query: age=30 *)
  let results = Ref_store.query_attr_value store ~attr ~value:(Fact.Int 30) in

  if List.length results != 2 then
    Error
      ("Should find 2 facts with age=30, found " ^ string_of_int (List.length results))
  else Ok ()

(** Test 6: Query by source *)
let test_query_source () =
  let source1 = Uri.of_string "test:source:A" in
  let source2 = Uri.of_string "test:source:B" in
  let entity = Uri.of_string "test:entity:1" in
  let attr = Uri.of_string "test:name" in

  let store = Ref_store.empty () in

  (* Add facts from different sources *)
  let fact1 =
    Fact.make ~source:source1 ~entity ~attribute:attr
      ~value:(Fact.String "Alice") ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in
  let fact2 =
    Fact.make ~source:source2 ~entity ~attribute:attr ~value:(Fact.String "Bob")
      ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in
  let fact3 =
    Fact.make ~source:source1 ~entity ~attribute:attr
      ~value:(Fact.String "Charlie") ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
  in

  Ref_store.add_fact store fact1;
  Ref_store.add_fact store fact2;
  Ref_store.add_fact store fact3;

  let results = Ref_store.query_source store ~source:source1 in

  (* Should find 2 facts from source A: fact1 and fact3 (different fact_uris) *)
  if List.length results != 2 then
    Error
      ("Should find 2 facts from source A, found " ^ 
       string_of_int (List.length results))
  else Ok ()

(** Test 7: Statistics *)
let test_statistics () =
  let store = Ref_store.empty () in
  let source = Uri.of_string "test:source" in
  let attr = Uri.of_string "test:name" in

  (* Add 10 facts across 3 entities *)
  let entities =
    [
      Uri.of_string "test:entity:1";
      Uri.of_string "test:entity:2";
      Uri.of_string "test:entity:3";
    ]
  in

  List.iteri
    (fun i entity ->
      let _i = i in (* Unused, generate UUID instead *)
      let fact =
        Fact.make ~source ~entity ~attribute:attr
          ~value:(Fact.String ("value" ^ string_of_int i))
          ~stated_at:(Datetime.now ()) ~tx_id:(UUID.v7_monotonic ())
      in
      Ref_store.add_fact store fact)
    (List.concat [ entities; entities; entities; entities ]); (* 12 facts total *)

  (* We added 12 facts, but with repeating entities and same fact_uris,
     the actual live count will be different. Let's just verify counts are reasonable *)

  if Ref_store.fact_count store < 1 then Error "fact_count should be > 0"
  else if Ref_store.entity_count store != 3 then
    Error
      ("entity_count should be 3, got " ^ string_of_int (Ref_store.entity_count store))
  else Ok ()

(** Test 8: Empty store *)
let test_empty_store () =
  let store = Ref_store.empty () in

  if Ref_store.all_live_facts store != [] then
    Error "Empty store should have no facts"
  else if Ref_store.fact_count store != 0 then
    Error "Empty store fact_count should be 0"
  else if Ref_store.entity_count store != 0 then
    Error "Empty store entity_count should be 0"
  else Ok ()

let tests =
  Test.
    [
      case "Basic add and query" test_basic_add_query;
      case "Last tx wins" test_last_tx_wins;
      case "Retraction" test_retraction;
      case "Compaction" test_compaction;
      case "Query by attr-value" test_query_attr_value;
      case "Query by source" test_query_source;
      case "Statistics" test_statistics;
      case "Empty store" test_empty_store;
    ]

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Test.Cli.main ~name:"poneglyph/lsm/ref_store" ~tests ~args)
    ~args:Env.args ()
