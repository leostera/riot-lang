(* Basic smoke tests for datalog core types *)
open Std
open Datalog

let test_value_comparison () =
  let v1 = Value.Int 42 in
  let v2 = Value.Int 42 in
  let v3 = Value.Int 43 in
  let v4 = Value.String "hello" in
  
  assert (Value.equal v1 v2);
  assert (not (Value.equal v1 v3));
  assert (not (Value.equal v1 v4));
  
  println "✓ Value comparison works"

let test_term_basics () =
  let var_x = Term.Var "X" in
  let const_42 = Term.Const (Value.Int 42) in
  let wildcard = Term.Wildcard in
  
  assert (Term.is_var var_x);
  assert (Term.is_const const_42);
  assert (Term.is_wildcard wildcard);
  
  assert (Term.var_name var_x = Some "X");
  assert (Term.const_value const_42 = Some (Value.Int 42));
  
  println "✓ Term basics work"

let test_relation_basics () =
  let open Collections in
  let rel = Relation.of_list [3; 1; 2; 1; 3] in
  
  (* Should be sorted and deduplicated: [1; 2; 3] *)
  assert (Relation.length rel = 3);
  assert (Relation.contains rel 1);
  assert (Relation.contains rel 2);
  assert (Relation.contains rel 3);
  assert (not (Relation.contains rel 4));
  
  let lst = Relation.to_list rel in
  assert (lst = [1; 2; 3]);
  
  println "✓ Relation basics work"

let test_relation_merge () =
  let rel1 = Relation.of_list [1; 2; 3] in
  let rel2 = Relation.of_list [3; 4; 5] in
  let merged = Relation.merge rel1 rel2 in
  
  let lst = Relation.to_list merged in
  assert (lst = [1; 2; 3; 4; 5]);
  assert (Relation.length merged = 5);
  
  println "✓ Relation merge works"

let test_relation_diff () =
  let rel1 = Relation.of_list [1; 2; 3; 4] in
  let rel2 = Relation.of_list [3; 4; 5] in
  let diff = Relation.diff rel1 rel2 in
  
  let lst = Relation.to_list diff in
  assert (lst = [1; 2]);
  
  println "✓ Relation diff works"

let test_ast_construction () =
  let edge = Ast.atom 
    ~predicate:"edge"
    ~args:[Term.Const (Value.Int 1); Term.Const (Value.Int 2)]
  in
  
  assert (edge.predicate = "edge");
  assert (List.length edge.args = 2);
  assert (Ast.is_ground edge);
  
  let path = Ast.atom
    ~predicate:"path"
    ~args:[Term.Var "X"; Term.Var "Y"]
  in
  
  assert (not (Ast.is_ground path));
  let vars = Ast.vars_in_atom path in
  assert (List.length vars = 2);
  assert (List.mem "X" vars);
  assert (List.mem "Y" vars);
  
  println "✓ AST construction works"

let () =
  println "Running datalog smoke tests...\n";
  
  test_value_comparison ();
  test_term_basics ();
  test_relation_basics ();
  test_relation_merge ();
  test_relation_diff ();
  test_ast_construction ();
  
  println "\n✅ All smoke tests passed!"
