(* Universe tests - simplified for Phase 0: Query-Only Datalog *)
open Std
open Datalog

let tests = [
  Test.case "InMemory storage - basic query" (fun () ->
      let universe = Universe.InMemory.of_facts [
        ("edge", [[Value.Int 1; Value.Int 2]; [Value.Int 2; Value.Int 3]]);
      ] in
      
      let facts = Universe.InMemory.get_facts_matching universe
        ~predicate:"edge"
        ~pattern:[None; None] in
      
      Test.assert_equal ~expected:2 ~actual:(Relation.length facts);
      Ok ());
  
  Test.case "InMemory storage - pattern matching" (fun () ->
      let universe = Universe.InMemory.of_facts [
        ("edge", [[Value.Int 1; Value.Int 2]; [Value.Int 2; Value.Int 3]]);
      ] in
      
      let facts = Universe.InMemory.get_facts_matching universe
        ~predicate:"edge"
        ~pattern:[Some (Value.Int 1); None] in
      
      Test.assert_equal ~expected:1 ~actual:(Relation.length facts);
      Ok ());
]
