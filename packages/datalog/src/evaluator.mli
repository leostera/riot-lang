open Std

(** {1 Evaluator - Query-Only Datalog}
    
    Phase 0: Query-Only Datalog Core
    
    The evaluator provides a thin query layer over storage snapshots.
    It supports:
    - Single-goal queries (one atom)
    - Multi-goal conjunctive queries (multiple atoms joined)
    - Pure streaming (zero materialization)
    
    It does NOT support:
    - Rules/derivations (use projection jobs instead)
    - Negation (use retraction at storage layer)
    - Builtins (filter in application code)
    
    {2 Example}
    
    {[
      (* Single-goal query *)
      let pattern = Ast.atom ~predicate:"language" 
        ~args:[Var "F"; Const (String "ocaml")] in
      
      module Eval = Evaluator.Make(Universe) in
      let results = Eval.query universe pattern in
      
      (* First result in <100ms! *)
      match Iter.MutIterator.next results with
      | Some sub -> println (Substitution.to_string sub)
      | None -> println "No matches"
      
      (* Multi-goal query *)
      let clauses = [
        Ast.Atom (Ast.atom ~predicate:"language" ~args:[Var "F"; Const (String "ocaml")]);
        Ast.Atom (Ast.atom ~predicate:"size" ~args:[Var "F"; Var "S"]);
      ] in
      
      let results = Eval.multi_query universe clauses in
      (* Streaming join - no materialization! *)
    ]}
*)

(** {2 Query Evaluation} *)

module Make (U : sig
  type t
  val get_facts_matching : t -> predicate:string -> pattern:Value.t option list -> Storage.fact_tuple Relation.t
end) : sig
  
  val query : U.t -> Ast.atom -> Substitution.t Iter.MutIterator.t
  (** Single-goal query - pure streaming!
      
      Returns an iterator of variable bindings (substitutions).
      Results are streamed on-demand with NO materialization.
      
      Uses storage index optimizations:
      - [None; Some value] → AVET index scan
      - [Some entity; None] → Entity lookup
      - _ → Full scan with filter
      
      Example:
      {[
        (* Query: language(F, "ocaml") *)
        let pattern = Ast.atom ~predicate:"language" 
          ~args:[Var "F"; Const (String "ocaml")] in
        
        module Eval = Evaluator.Make(Universe) in
        let results = Eval.query universe pattern in
        
        (* First result in <100ms, even with 1M facts! *)
        Iter.MutIterator.iter (fun sub ->
          println (Substitution.to_string sub)
        ) results
      ]}
  *)
  
  val multi_query : U.t -> Ast.clause list -> Substitution.t Iter.MutIterator.t
  (** Multi-goal conjunctive query - streaming join!
      
      Returns an iterator of variable bindings that satisfy ALL clauses.
      
      Strategy:
      - First atom is "driver" (streaming)
      - Filter by checking remaining atoms
      - No materialization - pure streaming!
      
      Limitations (Phase 0):
      - All clauses must be positive atoms (no negation)
      - No builtins (filter in application code)
      - First atom determines join order (no query planner)
      
      Example:
      {[
        (* Query: language(F, "ocaml"), size(F, S) *)
        let clauses = [
          Ast.Atom (Ast.atom ~predicate:"language" ~args:[Var "F"; Const (String "ocaml")]);
          Ast.Atom (Ast.atom ~predicate:"size" ~args:[Var "F"; Var "S"]);
        ] in
        
        module Eval = Evaluator.Make(Universe) in
        let results = Eval.multi_query universe clauses in
        
        (* Streams through language("ocaml") results, checks size for each *)
        Iter.MutIterator.iter (fun sub ->
          (* {F → "file:foo.ml", S → Int 1024} *)
          println (Substitution.to_string sub)
        ) results
      ]}
  *)
end
