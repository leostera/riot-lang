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
     
     Strategy: Start with first atom, then join with remaining atoms.
     Each join extends the substitution with new variable bindings.
     
     Pure streaming - no materialization except for relation iteration!
  *)
  let rec multi_query universe atoms =
    match atoms with
    | [] -> Iter.MutIterator.empty ()
    | first_atom :: rest_atoms ->
        (* Get first atom (must be positive) *)
        let first = match first_atom with
          | Ast.Atom a -> a
          | Ast.Negated _ -> panic "Multi-query cannot start with negated atom (not supported in Phase 0)"
          | Ast.Builtin _ -> panic "Multi-query cannot start with builtin (not supported in Phase 0)"
        in
        
        (* Get results from first atom *)
        let first_results = query universe first in
        
        (* Join with remaining atoms - extends substitution *)
        Iter.MutIterator.flat_map first_results ~fn:(fun sub ->
          join_remaining universe sub rest_atoms
        )
  
  (* Helper: Join substitution with remaining atoms
     
     For each remaining atom:
     1. Apply current substitution to instantiate bound variables
     2. Get matching facts from storage
     3. For each fact, unify with atom to extend substitution
     4. Recursively join with remaining atoms
  *)
  and join_remaining universe sub atoms =
    match atoms with
    | [] -> 
        (* No more atoms - return current substitution *)
        Iter.MutIterator.singleton sub
    
    | Ast.Atom atom :: rest ->
        (* Apply current substitution to atom *)
        let instantiated = Substitution.apply_to_atom sub atom in
        let pattern = atom_to_pattern instantiated in
        
        (* Get matching facts from storage *)
        let facts = U.get_facts_matching universe 
          ~predicate:atom.predicate ~pattern in
        
        (* For each matching fact, extend substitution and continue *)
        Iter.MutIterator.flat_map facts ~fn:(fun tuple ->
          (* Unify tuple with atom to extend substitution *)
          match Unify.match_atom sub atom tuple with
          | None -> Iter.MutIterator.empty ()  (* Unification failed *)
          | Some extended_sub ->
              (* Recursively join with remaining atoms *)
              join_remaining universe extended_sub rest
        )
    
    | Ast.Negated _ :: _ ->
        (* Negation not supported in Phase 0 - use retraction in storage instead *)
        panic "Negation not supported in Phase 0 queries (use retraction at storage layer)"
    
    | Ast.Builtin _ :: _ ->
        (* Builtins not supported in Phase 0 - filter in application code instead *)
        panic "Builtins not supported in Phase 0 queries (filter in application code)"
end
