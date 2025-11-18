open Std

module Make (U : sig
  type t
  val get_facts_matching : t -> predicate:string -> pattern:Value.t option list -> Storage.fact_tuple Relation.t
end) = struct
  
  (* Helper: convert atom args to storage pattern *)
  let atom_to_pattern atom =
    List.map (fun term ->
      match term with
      | Term.Const v -> Some v
      | Term.Var _ | Term.Wildcard -> None
    ) atom.Ast.args
  
  (* Single-goal query - pure streaming! *)
  let query universe pattern =
    let pred = pattern.Ast.predicate in
    let storage_pattern = atom_to_pattern pattern in
    
    (* Get facts using pattern - storage optimizes with indices! *)
    let matching_facts = U.get_facts_matching universe ~predicate:pred ~pattern:storage_pattern in
    
    (* Convert Relation to iterator and match - streaming! *)
    Unify.match_tuples_iter pattern matching_facts
  
  (* Multi-goal query - streaming join!
     
     Strategy: Take first atom as "driver", stream through its results,
     filter by checking if remaining atoms have matching facts.
     
     No materialization - pure streaming!
  *)
  let multi_query universe atoms =
    match atoms with
    | [] -> Iter.MutIterator.empty ()
    | first_atom :: rest_atoms ->
        (* Get first atom (must be positive) *)
        let first = match first_atom with
          | Ast.Atom a -> a
          | Ast.Negated _ -> panic "Multi-query cannot start with negated atom (not supported in Phase 0)"
          | Ast.Builtin _ -> panic "Multi-query cannot start with builtin (not supported in Phase 0)"
        in
        
        (* Stream through first atom results *)
        let first_results = query universe first in
        
        (* Filter by remaining atoms - streaming! *)
        Iter.MutIterator.filter first_results ~fn:(fun sub ->
          (* Check if all remaining atoms have matches *)
          List.for_all (fun clause ->
            match clause with
            | Ast.Atom atom ->
                (* Apply substitution to atom *)
                let instantiated = Substitution.apply_to_atom sub atom in
                let pattern = atom_to_pattern instantiated in
                
                (* Check if facts exist - peek, don't materialize! *)
                let facts = U.get_facts_matching universe 
                  ~predicate:atom.predicate ~pattern in
                not (Relation.is_empty facts)
            
            | Ast.Negated _ ->
                (* Negation not supported in Phase 0 - use retraction in storage instead *)
                panic "Negation not supported in Phase 0 queries (use retraction at storage layer)"
            
            | Ast.Builtin _ ->
                (* Builtins not supported in Phase 0 - filter in application code instead *)
                panic "Builtins not supported in Phase 0 queries (filter in application code)"
          ) rest_atoms
        )
end
