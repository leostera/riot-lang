(* Join tests *)
open Std
open Datalog

let tests =
  [
    (* Utility functions *)
    Test.case "atom_vars extracts variables" (fun () ->
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Const (Value.Int 1); Term.Var "Y"] in
        let vars = Join.atom_vars atom in
        Test.assert_equal ~expected:2 ~actual:(List.length vars);
        Test.assert_true (List.mem "X" vars);
        Test.assert_true (List.mem "Y" vars);
        Ok ());
    
    Test.case "shared_vars finds common variables" (fun () ->
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let atom2 = Ast.atom ~predicate:"path" 
          ~args:[Term.Var "Y"; Term.Var "Z"] in
        let shared = Join.shared_vars atom1 atom2 in
        Test.assert_equal ~expected:1 ~actual:(List.length shared);
        Test.assert_true (List.mem "Y" shared);
        Ok ());
    
    Test.case "shared_vars returns empty when no overlap" (fun () ->
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let atom2 = Ast.atom ~predicate:"path" 
          ~args:[Term.Var "A"; Term.Var "B"] in
        let shared = Join.shared_vars atom1 atom2 in
        Test.assert_equal ~expected:0 ~actual:(List.length shared);
        Ok ());
    
    (* Projection *)
    Test.case "project variables from substitution" (fun () ->
        let sub = Substitution.of_list [
          ("X", Value.Int 1);
          ("Y", Value.Int 2);
          ("Z", Value.Int 3);
        ] in
        match Join.project ~vars:["X"; "Z"] sub with
        | Some [Value.Int 1; Value.Int 3] -> Ok ()
        | _ -> Error "Should project X and Z");
    
    Test.case "project fails on unbound variable" (fun () ->
        let sub = Substitution.of_list [("X", Value.Int 1)] in
        match Join.project ~vars:["X"; "W"] sub with
        | None -> Ok ()
        | Some _ -> Error "Should fail when variable not bound");
    
    (* Cartesian product *)
    Test.case "cartesian product" (fun () ->
        let rel1 = Relation.of_list [[Value.Int 1]; [Value.Int 2]] in
        let rel2 = Relation.of_list [[Value.Int 3]; [Value.Int 4]] in
        let product = Join.cartesian_product rel1 rel2 in
        Test.assert_equal ~expected:4 ~actual:(List.length product);
        Ok ());
    
    (* Join operations *)
    Test.case "join atoms with shared variable" (fun () ->
        (* edge(X, Y) *)
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let rel1 = Relation.of_list [
          [Value.Int 1; Value.Int 2];
          [Value.Int 2; Value.Int 3];
        ] in
        
        (* path(Y, Z) *)
        let atom2 = Ast.atom ~predicate:"path" 
          ~args:[Term.Var "Y"; Term.Var "Z"] in
        let rel2 = Relation.of_list [
          [Value.Int 2; Value.Int 4];
          [Value.Int 3; Value.Int 5];
        ] in
        
        let results = Join.join_atoms atom1 rel1 atom2 rel2 in
        (* Should join on Y=2 and Y=3 *)
        Test.assert_equal ~expected:2 ~actual:(List.length results);
        Ok ());
    
    Test.case "join atoms produces correct tuples" (fun () ->
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let rel1 = Relation.of_list [[Value.Int 1; Value.Int 2]] in
        
        let atom2 = Ast.atom ~predicate:"path" 
          ~args:[Term.Var "Y"; Term.Var "Z"] in
        let rel2 = Relation.of_list [[Value.Int 2; Value.Int 3]] in
        
        let results = Join.join_atoms atom1 rel1 atom2 rel2 in
        Test.assert_equal ~expected:1 ~actual:(List.length results);
        
        match results with
        | [result] ->
            (* Check substitution *)
            let x = Substitution.lookup result.substitution ~var:"X" in
            let y = Substitution.lookup result.substitution ~var:"Y" in
            let z = Substitution.lookup result.substitution ~var:"Z" in
            
            let x_ok = match x with Some (Value.Int 1) -> true | _ -> false in
            let y_ok = match y with Some (Value.Int 2) -> true | _ -> false in
            let z_ok = match z with Some (Value.Int 3) -> true | _ -> false in
            
            if x_ok && y_ok && z_ok then Ok ()
            else Error "Variables not bound correctly"
        | _ -> Error "Should have exactly one result");
    
    Test.case "join with no shared variables" (fun () ->
        (* edge(X, Y) *)
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let rel1 = Relation.of_list [[Value.Int 1; Value.Int 2]] in
        
        (* node(Z) *)
        let atom2 = Ast.atom ~predicate:"node" 
          ~args:[Term.Var "Z"] in
        let rel2 = Relation.of_list [[Value.Int 3]; [Value.Int 4]] in
        
        let results = Join.join_atoms atom1 rel1 atom2 rel2 in
        (* At least some results from cartesian product *)
        let actual = List.length results in
        Test.assert_true (actual > 0);
        Ok ());
    
    Test.case "join with no matches returns empty" (fun () ->
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let rel1 = Relation.of_list [[Value.Int 1; Value.Int 2]] in
        
        (* Y=5 doesn't match anything *)
        let atom2 = Ast.atom ~predicate:"path" 
          ~args:[Term.Const (Value.Int 5); Term.Var "Z"] in
        let rel2 = Relation.of_list [[Value.Int 2; Value.Int 3]] in
        
        let results = Join.join_atoms atom1 rel1 atom2 rel2 in
        Test.assert_equal ~expected:0 ~actual:(List.length results);
        Ok ());
    
    Test.case "join with multiple matches" (fun () ->
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let rel1 = Relation.of_list [
          [Value.Int 1; Value.Int 2];
          [Value.Int 1; Value.Int 3];
        ] in
        
        let atom2 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "Y"; Term.Var "Z"] in
        let rel2 = Relation.of_list [
          [Value.Int 2; Value.Int 4];
          [Value.Int 3; Value.Int 4];
          [Value.Int 3; Value.Int 5];
        ] in
        
        let results = Join.join_atoms atom1 rel1 atom2 rel2 in
        let actual = List.length results in
        (* Should have multiple results from joining *)
        Test.assert_true (actual >= 2);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:join" ~tests ~args:Env.args)
    ~args:Env.args ()
