(* Unification tests *)
open Std
open Datalog

let tests =
  [
    (* Term unification *)
    Test.case "unify two equal constants" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Const (Value.Int 42) in
        let t2 = Term.Const (Value.Int 42) in
        
        match Unify.unify_terms sub t1 t2 with
        | Some _ -> Ok ()
        | None -> Error "Should unify equal constants");
    
    Test.case "unify two different constants fails" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Const (Value.Int 1) in
        let t2 = Term.Const (Value.Int 2) in
        
        match Unify.unify_terms sub t1 t2 with
        | None -> Ok ()
        | Some _ -> Error "Should not unify different constants");
    
    Test.case "unify variable with constant" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Var "X" in
        let t2 = Term.Const (Value.Int 42) in
        
        match Unify.unify_terms sub t1 t2 with
        | Some sub' ->
            (match Substitution.lookup sub' ~var:"X" with
            | Some (Value.Int 42) -> Ok ()
            | _ -> Error "X should be bound to 42")
        | None -> Error "Should unify variable with constant");
    
    Test.case "unify constant with variable" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Const (Value.String "hello") in
        let t2 = Term.Var "Y" in
        
        match Unify.unify_terms sub t1 t2 with
        | Some sub' ->
            (match Substitution.lookup sub' ~var:"Y" with
            | Some (Value.String "hello") -> Ok ()
            | _ -> Error "Y should be bound to 'hello'")
        | None -> Error "Should unify constant with variable");
    
    Test.case "unify two same variables" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Var "X" in
        let t2 = Term.Var "X" in
        
        match Unify.unify_terms sub t1 t2 with
        | Some _ -> Ok ()
        | None -> Error "Should unify same variable");
    
    Test.case "wildcard unifies with anything" (fun () ->
        let sub = Substitution.empty () in
        let t1 = Term.Wildcard in
        let t2 = Term.Const (Value.Int 99) in
        
        match Unify.unify_terms sub t1 t2 with
        | Some _ -> Ok ()
        | None -> Error "Wildcard should unify with constant");
    
    (* Term list unification *)
    Test.case "unify term lists" (fun () ->
        let sub = Substitution.empty () in
        let terms1 = [Term.Var "X"; Term.Const (Value.Int 2); Term.Var "Y"] in
        let terms2 = [Term.Const (Value.Int 1); Term.Const (Value.Int 2); Term.Const (Value.Int 3)] in
        
        match Unify.unify_terms_list sub terms1 terms2 with
        | Some sub' ->
            let x_ok = match Substitution.lookup sub' ~var:"X" with
              | Some (Value.Int 1) -> true
              | _ -> false
            in
            let y_ok = match Substitution.lookup sub' ~var:"Y" with
              | Some (Value.Int 3) -> true
              | _ -> false
            in
            if x_ok && y_ok then Ok ()
            else Error "Variables not bound correctly"
        | None -> Error "Should unify term lists");
    
    Test.case "unify term lists fails on length mismatch" (fun () ->
        let sub = Substitution.empty () in
        let terms1 = [Term.Var "X"; Term.Var "Y"] in
        let terms2 = [Term.Const (Value.Int 1)] in
        
        match Unify.unify_terms_list sub terms1 terms2 with
        | None -> Ok ()
        | Some _ -> Error "Should fail on length mismatch");
    
    (* Atom unification *)
    Test.case "unify atoms with same predicate" (fun () ->
        let sub = Substitution.empty () in
        let atom1 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Const (Value.Int 2)] in
        let atom2 = Ast.atom ~predicate:"edge" 
          ~args:[Term.Const (Value.Int 1); Term.Var "Y"] in
        
        match Unify.unify_atoms sub atom1 atom2 with
        | Some sub' ->
            let x_ok = match Substitution.lookup sub' ~var:"X" with
              | Some (Value.Int 1) -> true
              | _ -> false
            in
            let y_ok = match Substitution.lookup sub' ~var:"Y" with
              | Some (Value.Int 2) -> true
              | _ -> false
            in
            if x_ok && y_ok then Ok ()
            else Error "Variables not bound correctly"
        | None -> Error "Should unify atoms");
    
    Test.case "unify atoms fails on different predicates" (fun () ->
        let sub = Substitution.empty () in
        let atom1 = Ast.atom ~predicate:"edge" ~args:[Term.Var "X"] in
        let atom2 = Ast.atom ~predicate:"path" ~args:[Term.Var "X"] in
        
        match Unify.unify_atoms sub atom1 atom2 with
        | None -> Ok ()
        | Some _ -> Error "Should fail on different predicates");
    
    (* Match atom with tuple *)
    Test.case "match atom with tuple" (fun () ->
        let sub = Substitution.empty () in
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Const (Value.Int 2); Term.Var "Y"] in
        let tuple = [Value.Int 1; Value.Int 2; Value.String "label"] in
        
        match Unify.match_atom sub atom tuple with
        | Some sub' ->
            let x_ok = match Substitution.lookup sub' ~var:"X" with
              | Some (Value.Int 1) -> true
              | _ -> false
            in
            let y_ok = match Substitution.lookup sub' ~var:"Y" with
              | Some (Value.String "label") -> true
              | _ -> false
            in
            if x_ok && y_ok then Ok ()
            else Error "Variables not bound correctly"
        | None -> Error "Should match atom with tuple");
    
    Test.case "match atom fails on constant mismatch" (fun () ->
        let sub = Substitution.empty () in
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Const (Value.Int 5)] in
        let tuple = [Value.Int 1; Value.Int 2] in
        
        match Unify.match_atom sub atom tuple with
        | None -> Ok ()
        | Some _ -> Error "Should fail when constant doesn't match");
    
    Test.case "match atom fails on arity mismatch" (fun () ->
        let sub = Substitution.empty () in
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let tuple = [Value.Int 1] in
        
        match Unify.match_atom sub atom tuple with
        | None -> Ok ()
        | Some _ -> Error "Should fail on arity mismatch");
    
    (* Match atoms with relation *)
    Test.case "match atoms with relation" (fun () ->
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Var "X"; Term.Var "Y"] in
        let facts = Relation.of_list [
          [Value.Int 1; Value.Int 2];
          [Value.Int 2; Value.Int 3];
          [Value.Int 3; Value.Int 4];
        ] in
        
        let subs = Unify.match_atoms atom facts in
        Test.assert_equal ~expected:3 ~actual:(List.length subs);
        Ok ());
    
    (* Grounding *)
    Test.case "ground term with substitution" (fun () ->
        let sub = Substitution.of_list [("X", Value.Int 42)] in
        let term = Term.Var "X" in
        
        match Unify.ground sub term with
        | Some (Value.Int 42) -> Ok ()
        | _ -> Error "Should ground variable to value");
    
    Test.case "ground fails on unbound variable" (fun () ->
        let sub = Substitution.empty () in
        let term = Term.Var "Z" in
        
        match Unify.ground sub term with
        | None -> Ok ()
        | Some _ -> Error "Should fail on unbound variable");
    
    Test.case "ground tuple" (fun () ->
        let sub = Substitution.of_list [
          ("X", Value.Int 1);
          ("Y", Value.Int 2);
        ] in
        let terms = [Term.Var "X"; Term.Var "Y"; Term.Const (Value.Int 3)] in
        
        match Unify.ground_tuple sub terms with
        | Some [Value.Int 1; Value.Int 2; Value.Int 3] -> Ok ()
        | _ -> Error "Should ground tuple");
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:unify" ~tests ~args:Env.args)
    ~args:Env.args ()
