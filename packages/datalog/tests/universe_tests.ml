(* Universe and Storage tests *)
open Std
open Datalog

let tests =
  [
    (* InMemory Storage tests *)
    Test.case "inmemory storage - add and get facts" (fun () ->
        let storage = InmemoryStorage.create () in
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 2; Value.Int 3];
        
        let facts = InmemoryStorage.get_facts storage ~predicate:"edge" in
        Test.assert_equal ~expected:2 ~actual:(Relation.length facts);
        Ok ());
    
    Test.case "inmemory storage - deduplication" (fun () ->
        let storage = InmemoryStorage.create () in
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];  (* Duplicate *)
        
        let facts = InmemoryStorage.get_facts storage ~predicate:"edge" in
        Test.assert_equal ~expected:1 ~actual:(Relation.length facts);
        Ok ());
    
    Test.case "inmemory storage - predicates list" (fun () ->
        let storage = InmemoryStorage.create () in
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];
        InmemoryStorage.add_fact storage ~predicate:"person" 
          ~tuple:[Value.String "alice"; Value.Int 30];
        
        let preds = InmemoryStorage.predicates storage in
        Test.assert_equal ~expected:2 ~actual:(List.length preds);
        Ok ());
    
    (* Substitution tests *)
    Test.case "substitution - bind and lookup" (fun () ->
        let sub = Substitution.empty () in
        let sub = Substitution.bind sub ~var:"X" ~value:(Value.Int 42) in
        
        match Substitution.lookup sub ~var:"X" with
        | Some (Value.Int 42) -> Ok ()
        | _ -> Error "Expected Int 42");
    
    Test.case "substitution - apply to term" (fun () ->
        let sub = Substitution.empty () in
        let sub = Substitution.bind sub ~var:"X" ~value:(Value.Int 42) in
        
        let term = Term.Var "X" in
        let result = Substitution.apply_to_term sub term in
        
        match result with
        | Term.Const (Value.Int 42) -> Ok ()
        | _ -> Error "Expected Const (Int 42)");
    
    Test.case "substitution - apply to atom" (fun () ->
        let sub = Substitution.of_list [
          ("X", Value.Int 1);
          ("Y", Value.Int 2);
        ] in
        
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let result = Substitution.apply_to_atom sub atom in
        
        Test.assert_equal ~expected:"edge" ~actual:result.predicate;
        match result.args with
        | [Term.Const (Value.Int 1); Term.Const (Value.Int 2)] -> Ok ()
        | _ -> Error "Expected edge(1, 2)");
    
    Test.case "substitution - merge compatible" (fun () ->
        let sub1 = Substitution.of_list [("X", Value.Int 1)] in
        let sub2 = Substitution.of_list [("Y", Value.Int 2)] in
        
        match Substitution.merge sub1 sub2 with
        | Some merged -> 
            Test.assert_equal ~expected:2 ~actual:(Substitution.size merged);
            Ok ()
        | None -> Error "Merge should succeed");
    
    Test.case "substitution - merge conflict" (fun () ->
        let sub1 = Substitution.of_list [("X", Value.Int 1)] in
        let sub2 = Substitution.of_list [("X", Value.Int 2)] in
        
        match Substitution.merge sub1 sub2 with
        | None -> Ok ()
        | Some _ -> Error "Merge should fail due to conflict");
    
    (* Universe tests *)
    Test.case "universe - create and add rules" (fun () ->
        let universe = Universe.InMemory.create_empty () in
        
        let rule = Ast.rule 
          ~head:(Ast.atom ~predicate:"path" ~args:[Term.Var "X"; Term.Var "Y"])
          ~body:[Ast.Atom (Ast.atom ~predicate:"edge" ~args:[Term.Var "X"; Term.Var "Y"])]
        in
        
        let universe = Universe.InMemory.add_rule universe rule in
        let rules = Universe.InMemory.rules universe in
        
        Test.assert_equal ~expected:1 ~actual:(List.length rules);
        Ok ());
    
    Test.case "universe - base and derived facts" (fun () ->
        let storage = InmemoryStorage.create () in
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];
        
        let universe = Universe.InMemory.create storage in
        
        (* Add derived fact *)
        Universe.InMemory.add_derived_fact universe 
          ~predicate:"path" ~tuple:[Value.Int 1; Value.Int 2];
        
        (* Check base facts *)
        let base = Universe.InMemory.get_base_facts universe ~predicate:"edge" in
        Test.assert_equal ~expected:1 ~actual:(Relation.length base);
        
        (* Check derived facts *)
        let derived = Universe.InMemory.get_derived_facts universe ~predicate:"path" in
        Test.assert_equal ~expected:1 ~actual:(Relation.length derived);
        
        Ok ());
    
    Test.case "universe - of_facts convenience" (fun () ->
        let universe = Universe.InMemory.of_facts [
          ("edge", [[Value.Int 1; Value.Int 2]; [Value.Int 2; Value.Int 3]]);
          ("person", [[Value.String "alice"; Value.Int 30]]);
        ] in
        
        let edge_facts = Universe.InMemory.get_facts universe ~predicate:"edge" in
        Test.assert_equal ~expected:2 ~actual:(Relation.length edge_facts);
        
        let person_facts = Universe.InMemory.get_facts universe ~predicate:"person" in
        Test.assert_equal ~expected:1 ~actual:(Relation.length person_facts);
        
        Ok ());
    
    Test.case "universe - predicates lists all" (fun () ->
        let storage = InmemoryStorage.create () in
        InmemoryStorage.add_fact storage ~predicate:"edge" 
          ~tuple:[Value.Int 1; Value.Int 2];
        
        let universe = Universe.InMemory.create storage in
        Universe.InMemory.add_derived_fact universe 
          ~predicate:"path" ~tuple:[Value.Int 1; Value.Int 2];
        
        let all_preds = Universe.InMemory.predicates universe in
        (* Should have both 'edge' and 'path' *)
        Test.assert_true (List.length all_preds >= 2);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:universe" ~tests ~args:Env.args)
    ~args:Env.args ()
