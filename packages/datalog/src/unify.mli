open Std

(** {1 Unify - Pattern Matching and Unification}
    
    Unification is the process of finding substitutions that make two terms
    or atoms equal. This is the core of Datalog's pattern matching.
    
    {2 Examples}
    
    {[
      (* Unify constant with constant *)
      unify_terms sub (Const (Int 1)) (Const (Int 1))  
      (* Some sub - success *)
      
      unify_terms sub (Const (Int 1)) (Const (Int 2))  
      (* None - conflict *)
      
      (* Unify variable with constant *)
      unify_terms sub (Var "X") (Const (Int 42))  
      (* Some sub' where X → 42 *)
      
      (* Unify two variables *)
      unify_terms sub (Var "X") (Var "Y")  
      (* Some sub' where X → Y (or vice versa) *)
      
      (* Match atom with tuple *)
      let atom = atom ~predicate:"edge" ~args:[Var "X"; Const (Int 2)] in
      match_atom sub atom [Int 1; Int 2]
      (* Some sub' where X → 1 *)
    ]}
*)

(** {2 Term Unification} *)

val unify_terms : Substitution.t -> Term.t -> Term.t -> Substitution.t option
(** Unify two terms, extending the given substitution.
    Returns [None] if unification fails (conflict).
    
    Unification rules:
    - [Const v1] with [Const v2]: succeeds if v1 = v2
    - [Var x] with [Const v]: bind x to v (if not already bound differently)
    - [Var x] with [Var y]: bind x to y (or check consistency)
    - [Wildcard] with anything: always succeeds (no binding)
    - Terms with substitutions applied first
    
    Example:
    {[
      let sub = Substitution.empty () in
      let sub = unify_terms sub (Var "X") (Const (Int 42)) in
      match sub with
      | Some sub' -> 
          (* X is now bound to 42 *)
          Substitution.lookup sub' ~var:"X"  (* Some (Int 42) *)
      | None -> (* Unification failed *)
    ]}
*)

val unify_terms_list : Substitution.t -> Term.t list -> Term.t list -> Substitution.t option
(** Unify two lists of terms pairwise.
    Returns [None] if lists have different lengths or any pair fails to unify.
    
    Example:
    {[
      unify_terms_list sub 
        [Var "X"; Var "Y"] 
        [Const (Int 1); Const (Int 2)]
      (* Some sub' where X→1, Y→2 *)
    ]}
*)

(** {2 Atom Unification} *)

val unify_atoms : Substitution.t -> Ast.atom -> Ast.atom -> Substitution.t option
(** Unify two atoms.
    Returns [None] if:
    - Predicates don't match
    - Argument counts differ
    - Any argument fails to unify
    
    Example:
    {[
      let atom1 = atom ~predicate:"edge" ~args:[Var "X"; Const (Int 2)] in
      let atom2 = atom ~predicate:"edge" ~args:[Const (Int 1); Var "Y"] in
      
      match unify_atoms sub atom1 atom2 with
      | Some sub' -> 
          (* X→1, Y→2 *)
      | None -> (* Failed *)
    ]}
*)

(** {2 Matching Atoms with Tuples} *)

val match_atom : Substitution.t -> Ast.atom -> Storage.fact_tuple -> Substitution.t option
(** Match an atom pattern against a concrete tuple of values.
    Returns [None] if:
    - Arities don't match
    - Any constant in atom doesn't match corresponding value in tuple
    - Variable bindings conflict with existing substitution
    
    This is the key operation for querying: we have a pattern (atom with variables)
    and we want to match it against concrete facts (tuples of values).
    
    Example:
    {[
      let sub = Substitution.empty () in
      let pattern = atom ~predicate:"edge" 
        ~args:[Var "X"; Const (Int 2); Var "Y"] in
      let tuple = [Int 1; Int 2; String "label"] in
      
      match match_atom sub pattern tuple with
      | Some sub' ->
          (* X→1, Y→"label" *)
          (* Const (Int 2) matched Int 2 in tuple *)
      | None -> (* No match *)
    ]}
*)

val match_atoms : Ast.atom -> Storage.fact_tuple Relation.t -> Substitution.t list
(** Match an atom against a relation of tuples.
    Returns list of substitutions, one for each matching tuple.
    
    This is batch matching - useful for querying a predicate.
    
    Example:
    {[
      let pattern = atom ~predicate:"edge" ~args:[Var "X"; Var "Y"] in
      let facts = Relation.of_list [
        [Int 1; Int 2];
        [Int 2; Int 3];
        [Int 3; Int 4];
      ] in
      
      let subs = match_atoms pattern facts in
      (* Returns 3 substitutions:
         [X→1, Y→2]
         [X→2, Y→3]
         [X→3, Y→4]
      *)
    ]}
*)

val match_atoms_iter : Ast.atom -> Storage.fact_tuple Relation.t -> Substitution.t Iter.MutIterator.t
(** Match an atom against a relation of tuples - streaming version.
    Returns an iterator that produces substitutions on-demand.
    
    This enables streaming query results without materializing all matches.
    Results are produced lazily as the iterator is consumed.
    
    Example:
    {[
      let pattern = atom ~predicate:"edge" ~args:[Var "X"; Var "Y"] in
      let facts = get_facts ~predicate:"edge" in
      
      let iter = match_atoms_iter pattern facts in
      (* Results produced on-demand as you call next() *)
      Iter.MutIterator.iter (fun sub ->
        println (Substitution.to_string sub)
      ) iter
    ]}
*)

val match_tuples_iter : Ast.atom -> Storage.fact_tuple Iter.MutIterator.t -> Substitution.t Iter.MutIterator.t
(** Match an atom against a stream of tuples - pure streaming, NO materialization!
    
    This is the fully streaming version for query execution.
    Unlike match_atoms_iter which takes a Relation (materialized), this takes
    a MutIterator and never materializes the full dataset.
    
    This eliminates the Relation.to_list bottleneck in the query path.
    
    Example:
    {[
      let pattern = atom ~predicate:"edge" ~args:[Var "X"; Var "Y"] in
      let tuples_iter = get_facts_iter ~predicate:"edge" in
      
      let results = match_tuples_iter pattern tuples_iter in
      (* Pure streaming - first result available immediately! *)
      match Iter.MutIterator.next results with
      | Some sub -> println (Substitution.to_string sub)
      | None -> println "No matches"
    ]}
*)

(** {2 Utilities} *)

val occurs_check : var:string -> Term.t -> bool
(** Check if variable occurs in term (prevents infinite structures).
    Returns [true] if variable appears in term.
    
    Example:
    {[
      occurs_check ~var:"X" (Var "X")  (* true *)
      occurs_check ~var:"X" (Var "Y")  (* false *)
      occurs_check ~var:"X" (Const (Int 1))  (* false *)
    ]}
    
    Used internally to prevent creating cyclic bindings like X → f(X).
*)

val ground : Substitution.t -> Term.t -> Value.t option
(** Fully ground a term using substitution.
    Returns [Some value] if term can be reduced to a constant.
    Returns [None] if term still contains unbound variables.
    
    Example:
    {[
      let sub = Substitution.of_list [("X", Int 42)] in
      ground sub (Var "X")  (* Some (Int 42) *)
      ground sub (Var "Y")  (* None - Y not bound *)
      ground sub (Const (Int 1))  (* Some (Int 1) *)
    ]}
*)

val ground_tuple : Substitution.t -> Term.t list -> Storage.fact_tuple option
(** Ground a tuple of terms.
    Returns [Some tuple] if all terms can be grounded.
    Returns [None] if any term contains unbound variables.
    
    Example:
    {[
      let sub = Substitution.of_list [("X", Int 1); ("Y", Int 2)] in
      ground_tuple sub [Var "X"; Var "Y"; Const (Int 3)]
      (* Some [Int 1; Int 2; Int 3] *)
      
      ground_tuple sub [Var "X"; Var "Z"]
      (* None - Z not bound *)
    ]}
*)
