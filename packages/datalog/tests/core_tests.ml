(* Core unit tests for datalog types *)
open Std
open Datalog

let tests =
  [
    (* Value tests *)
    Test.case "value equality" (fun () ->
        Test.assert_true (Value.equal (Value.Int 42) (Value.Int 42));
        Test.assert_false (Value.equal (Value.Int 42) (Value.Int 43));
        Ok ());
    
    (* Term tests *)
    Test.case "term predicates" (fun () ->
        Test.assert_true (Term.is_var (Term.Var "X"));
        Test.assert_true (Term.is_const (Term.Const (Value.Int 1)));
        Test.assert_true (Term.is_wildcard Term.Wildcard);
        Ok ());
    
    (* Relation tests *)
    Test.case "relation sort and dedup" (fun () ->
        let rel = Relation.of_list [3; 1; 2; 1; 3] in
        Test.assert_equal ~expected:[1; 2; 3] ~actual:(Relation.to_list rel);
        Ok ());
    
    Test.case "relation merge" (fun () ->
        let r1 = Relation.of_list [1; 2; 3] in
        let r2 = Relation.of_list [3; 4; 5] in
        let merged = Relation.merge r1 r2 in
        Test.assert_equal ~expected:[1; 2; 3; 4; 5] ~actual:(Relation.to_list merged);
        Ok ());
    
    (* AST tests *)
    Test.case "ast atom construction" (fun () ->
        let atom = Ast.atom ~predicate:"edge" 
          ~args:[Term.Const (Value.Int 1); Term.Const (Value.Int 2)] in
        Test.assert_equal ~expected:"edge" ~actual:atom.predicate;
        Test.assert_true (Ast.is_ground atom);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:core" ~tests ~args:Env.args)
    ~args:Env.args ()
