open Std

(** {1 Evaluator - Fixed-Point Rule Evaluation}
    
    The evaluator takes a universe (base facts + rules) and computes all derivable
    facts by iterating rules to a fixed point.
    
    {2 Algorithm}
    
    We use semi-naive evaluation:
    1. Start with base facts as "recent"
    2. For each iteration:
       - Apply rules using recent facts
       - Add new derived facts to recent
       - Move recent → stable
    3. Stop when no new facts are derived (fixed point)
    
    {2 Example}
    
    {[
      (* Base facts *)
      edge(1, 2).
      edge(2, 3).
      
      (* Rules *)
      path(X, Y) :- edge(X, Y).
      path(X, Z) :- edge(X, Y), path(Y, Z).
      
      (* Evaluation *)
      Iteration 1:
        path(1, 2) from edge(1, 2)
        path(2, 3) from edge(2, 3)
      
      Iteration 2:
        path(1, 3) from edge(1, 2), path(2, 3)
      
      Iteration 3:
        No new facts - done!
      
      Result: path = {(1,2), (2,3), (1,3)}
    ]}
*)

(** {2 Evaluation} *)

module Make (U : sig
  type t
  val get_facts : t -> predicate:string -> Storage.fact_tuple Relation.t
  val add_derived_facts : t -> predicate:string -> tuples:Storage.fact_tuple Relation.t -> unit
  val rules : t -> Ast.rule list
end) : sig
  
  val eval_rule : U.t -> Ast.rule -> Storage.fact_tuple Relation.t
  (** Evaluate a single rule against current universe.
      Returns newly derived facts (not facts already in universe).
      
      This is the core: join rule body atoms, project to head variables.
  *)
  
  val eval : U.t -> U.t
  (** Evaluate all rules to fixed point.
      Iterates until no new facts are derived.
      
      Example:
      {[
        let universe = Universe.InMemory.of_facts [
          ("edge", [[Int 1; Int 2]; [Int 2; Int 3]]);
        ] in
        
        let universe = Universe.InMemory.add_rule universe rule1 in
        let universe = Universe.InMemory.add_rule universe rule2 in
        
        module Eval = Evaluator.Make(Universe.InMemory) in
        let universe = Eval.eval universe in
        
        (* Now universe contains all derived facts *)
        let all_paths = Universe.InMemory.get_facts universe ~predicate:"path" in
        ...
      ]}
  *)
  
  val query : U.t -> Ast.atom -> Substitution.t list
  (** Query universe for matches to an atom pattern.
      Returns list of variable bindings (substitutions).
      
      Example:
      {[
        (* Query: path(1, Y) - what can we reach from 1? *)
        let pattern = Ast.atom ~predicate:"path" 
          ~args:[Const (Int 1); Var "Y"] in
        
        module Eval = Evaluator.Make(Universe.InMemory) in
        let results = Eval.query universe pattern in
        
        (* Results: [{Y→2}, {Y→3}, {Y→4}] *)
        List.iter (fun sub ->
          match Substitution.lookup sub ~var:"Y" with
          | Some value -> println (Value.to_string value)
          | None -> ()
        ) results
      ]}
  *)
  
  val multi_query : U.t -> Ast.clause list -> Substitution.t list
  (** Query universe with multiple atoms (join query).
      Returns list of variable bindings that satisfy all clauses.
      
      This is equivalent to evaluating a rule body without projecting to a head.
      
      Example:
      {[
        (* Query: parent(X, Y), age(X, A) - parents with their ages *)
        let clauses = [
          Ast.Atom (Ast.atom ~predicate:"parent" ~args:[Var "X"; Var "Y"]);
          Ast.Atom (Ast.atom ~predicate:"age" ~args:[Var "X"; Var "A"]);
        ] in
        
        module Eval = Evaluator.Make(Universe.InMemory) in
        let results = Eval.multi_query universe clauses in
        
        (* Results: [{X→"alice", Y→"bob", A→60}, {X→"bob", Y→"charlie", A→40}, ...] *)
      ]}
  *)
end
