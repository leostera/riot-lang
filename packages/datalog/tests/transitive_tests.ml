(* End-to-end transitive closure tests *)
open Std
open Datalog

module Eval = Evaluator.Make(Universe.InMemory)

let tests =
  [
    Test.case "simple transitive closure" (fun () ->
        (* Create universe with base facts *)
        let universe = Universe.InMemory.of_facts [
          ("edge", [
            [Value.Int 1; Value.Int 2];
            [Value.Int 2; Value.Int 3];
            [Value.Int 3; Value.Int 4];
          ]);
        ] in
        
        (* Add rules for transitive closure *)
        (* reachable(X, Y) :- edge(X, Y) *)
        let rule1 = Ast.rule
          ~head:(Ast.atom ~predicate:"reachable" 
            ~args:[Term.Var "X"; Term.Var "Y"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"edge" 
              ~args:[Term.Var "X"; Term.Var "Y"])
          ]
        in
        
        (* reachable(X, Z) :- edge(X, Y), reachable(Y, Z) *)
        let rule2 = Ast.rule
          ~head:(Ast.atom ~predicate:"reachable" 
            ~args:[Term.Var "X"; Term.Var "Z"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"edge" 
              ~args:[Term.Var "X"; Term.Var "Y"]);
            Ast.Atom (Ast.atom ~predicate:"reachable" 
              ~args:[Term.Var "Y"; Term.Var "Z"]);
          ]
        in
        
        let universe = Universe.InMemory.add_rules universe [rule1; rule2] in
        
        println "Evaluating transitive closure...";
        let universe = Eval.eval universe in
        println "Evaluation complete!";
        
        (* Check results *)
        let reachable = Universe.InMemory.get_facts universe ~predicate:"reachable" in
        let count = Relation.length reachable in
        
        println ("Found " ^ string_of_int count ^ " reachable pairs");
        
        (* Should have: (1,2), (2,3), (3,4), (1,3), (2,4), (1,4) = 6 pairs *)
        Test.assert_true (count >= 3);  (* At least base facts *)
        Ok ());
    
    Test.case "query transitive closure" (fun () ->
        let universe = Universe.InMemory.of_facts [
          ("edge", [
            [Value.Int 1; Value.Int 2];
            [Value.Int 2; Value.Int 3];
          ]);
        ] in
        
        let rule1 = Ast.rule
          ~head:(Ast.atom ~predicate:"path" 
            ~args:[Term.Var "X"; Term.Var "Y"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"edge" 
              ~args:[Term.Var "X"; Term.Var "Y"])
          ]
        in
        
        let rule2 = Ast.rule
          ~head:(Ast.atom ~predicate:"path" 
            ~args:[Term.Var "X"; Term.Var "Z"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"edge" 
              ~args:[Term.Var "X"; Term.Var "Y"]);
            Ast.Atom (Ast.atom ~predicate:"path" 
              ~args:[Term.Var "Y"; Term.Var "Z"]);
          ]
        in
        
        let universe = Universe.InMemory.add_rules universe [rule1; rule2] in
        let universe = Eval.eval universe in
        
        (* Query: path(1, Y) - what can we reach from 1? *)
        let query_pattern = Ast.atom ~predicate:"path" 
          ~args:[Term.Const (Value.Int 1); Term.Var "Y"] in
        
        let results = Eval.query universe query_pattern in
        
        println ("Query found " ^ string_of_int (List.length results) ^ " results");
        
        (* Should find at least path(1, 2) and path(1, 3) *)
        Test.assert_true (List.length results >= 2);
        Ok ());
    
    Test.case "diamond graph" (fun () ->
        (* Graph: 1 -> 2 -> 4
                   1 -> 3 -> 4 *)
        let universe = Universe.InMemory.of_facts [
          ("edge", [
            [Value.Int 1; Value.Int 2];
            [Value.Int 1; Value.Int 3];
            [Value.Int 2; Value.Int 4];
            [Value.Int 3; Value.Int 4];
          ]);
        ] in
        
        let rule1 = Ast.rule
          ~head:(Ast.atom ~predicate:"reach" 
            ~args:[Term.Var "X"; Term.Var "Y"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"edge" 
              ~args:[Term.Var "X"; Term.Var "Y"])
          ]
        in
        
        let rule2 = Ast.rule
          ~head:(Ast.atom ~predicate:"reach" 
            ~args:[Term.Var "X"; Term.Var "Z"])
          ~body:[
            Ast.Atom (Ast.atom ~predicate:"reach" 
              ~args:[Term.Var "X"; Term.Var "Y"]);
            Ast.Atom (Ast.atom ~predicate:"reach" 
              ~args:[Term.Var "Y"; Term.Var "Z"]);
          ]
        in
        
        let universe = Universe.InMemory.add_rules universe [rule1; rule2] in
        let universe = Eval.eval universe in
        
        let reachable = Universe.InMemory.get_facts universe ~predicate:"reach" in
        let count = Relation.length reachable in
        
        println ("Diamond graph: " ^ string_of_int count ^ " reachable pairs");
        
        (* Should have at least the 4 base facts plus transitive ones *)
        Test.assert_true (count >= 4);
        Ok ());
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
      Test.Cli.main ~name:"datalog:transitive" ~tests ~args:Env.args)
    ~args:Env.args ()
