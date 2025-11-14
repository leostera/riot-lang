open Std

module Make (U : sig
  type t
  val get_facts : t -> predicate:string -> Storage.fact_tuple Relation.t
  val add_derived_facts : t -> predicate:string -> tuples:Storage.fact_tuple Relation.t -> unit
  val rules : t -> Ast.rule list
end) = struct
  
  (* Evaluate a single body atom to get its facts *)
  let eval_body_atom universe atom =
    match atom with
    | Ast.Atom a -> U.get_facts universe ~predicate:a.predicate
    | Ast.Negated _ -> Relation.empty ()  (* TODO: Implement negation *)
    | Ast.Builtin _ -> Relation.empty ()   (* TODO: Implement builtins *)
  
  (* Check if a negated atom holds (for negation as failure) *)
  let eval_negated_atom universe sub neg_atom =
    (* Apply substitution to the negated atom *)
    let instantiated = Substitution.apply_to_atom sub neg_atom in
    
    (* Get facts for this predicate *)
    let facts = U.get_facts universe ~predicate:neg_atom.Ast.predicate in
    
    (* Check if the instantiated atom matches any fact *)
    let matches = Unify.match_atoms instantiated facts in
    
    (* Negation succeeds if NO facts match *)
    List.is_empty matches
  
  (* Evaluate a builtin constraint *)
  let eval_builtin sub op terms =
    (* Apply substitution to all terms first *)
    let terms = List.map (Substitution.apply_to_term sub) terms in
    
    (* Try to ground the terms to values *)
    let values = List.filter_map (fun term ->
      match term with
      | Term.Const v -> Some v
      | Term.Var _ -> None  (* Unbound variable - can't evaluate *)
      | Term.Wildcard -> None
    ) terms in
    
    (* If any term is still a variable, we can't evaluate the builtin *)
    if List.length values != List.length terms then
      false
    else
      (* Evaluate the builtin operator *)
      match op, values with
      (* Equality *)
      | "=", [v1; v2] -> Value.equal v1 v2
      
      (* Inequality *)
      | "!=", [v1; v2] -> not (Value.equal v1 v2)
      
      (* Comparisons - only work on integers *)
      | ">", [Value.Int a; Value.Int b] -> a > b
      | "<", [Value.Int a; Value.Int b] -> a < b
      | ">=", [Value.Int a; Value.Int b] -> a >= b
      | "<=", [Value.Int a; Value.Int b] -> a <= b
      
      (* Unknown operator or type mismatch *)
      | _ -> 
          panic ("Unsupported builtin: " ^ op ^ " with " ^ 
                 string_of_int (List.length values) ^ " args")
  
  (* Evaluate rule body by joining all atoms *)
  let eval_rule_body universe body =
    match body with
    | [] -> []  (* No body means head is always true - not standard *)
    | first_clause :: rest_clauses ->
        (* Start with first atom - must be positive *)
        let first_atom = match first_clause with
          | Ast.Atom a -> a
          | Ast.Negated _ -> panic "Rule body cannot start with negated atom (stratification required)"
          | Ast.Builtin _ -> panic "Rule body cannot start with builtin"
        in
        
        let first_facts = eval_body_atom universe first_clause in
        let first_matches = Unify.match_atoms first_atom first_facts in
        
        (* Join with remaining atoms *)
        let rec join_remaining subs clauses_remaining =
          match clauses_remaining with
          | [] -> subs
          | clause :: rest -> (
              match clause with
              | Ast.Atom atom ->
                  (* Positive atom - standard join *)
                  let facts = eval_body_atom universe clause in
                  
                  (* For each existing substitution, extend with this atom *)
                  let rec extend_subs acc remaining_subs =
                    match remaining_subs with
                    | [] -> acc
                    | sub :: rest_subs ->
                        (* Apply current substitution to atom *)
                        let instantiated_atom = Substitution.apply_to_atom sub atom in
                        
                        (* Try to match against facts *)
                        let matches = Unify.match_atoms instantiated_atom facts in
                        
                        (* Merge each match with current substitution *)
                        let rec merge_matches acc2 remaining_matches =
                          match remaining_matches with
                          | [] -> acc2
                          | match_sub :: rest_matches ->
                              (match Substitution.merge sub match_sub with
                              | Some merged -> merge_matches (merged :: acc2) rest_matches
                              | None -> merge_matches acc2 rest_matches)
                        in
                        
                        let extended = merge_matches [] matches in
                        extend_subs (List.rev_append extended acc) rest_subs
                  in
                  
                  let new_subs = extend_subs [] subs in
                  join_remaining new_subs rest
                  
              | Ast.Negated neg_atom ->
                  (* Negated atom - filter substitutions where atom does NOT hold *)
                  let filtered = List.filter (fun sub ->
                    eval_negated_atom universe sub neg_atom
                  ) subs in
                  join_remaining filtered rest
                  
              | Ast.Builtin (op, terms) ->
                  (* Builtin constraint - filter substitutions that satisfy it *)
                  let filtered = List.filter (fun sub ->
                    eval_builtin sub op terms
                  ) subs in
                  join_remaining filtered rest
            )
        in
        
        join_remaining first_matches rest_clauses
  
  (* Evaluate a single rule *)
  let eval_rule universe rule =
    (* Evaluate body to get substitutions *)
    let body_subs = eval_rule_body universe rule.Ast.body in
    
    (* Project each substitution to head variables *)
    let head = rule.Ast.head in
    let head_vars = Join.atom_vars head in
    
    (* Ground each substitution to produce fact tuples *)
    let rec ground_subs acc remaining =
      match remaining with
      | [] -> acc
      | sub :: rest ->
          (match Join.project ~vars:head_vars sub with
          | Some tuple -> ground_subs (tuple :: acc) rest
          | None -> ground_subs acc rest)
    in
    
    let new_tuples = ground_subs [] body_subs in
    Relation.of_list new_tuples
  
  (* Evaluate all rules to fixed point *)
  let eval universe =
    let max_iterations = 1000 in  (* Safety limit *)
    
    let rec iterate iteration =
      if iteration > max_iterations then
        panic "Maximum iterations exceeded - possible infinite loop"
      else begin
        let changed = ref false in
        
        (* Evaluate each rule *)
        let rules = U.rules universe in
        List.iter (fun rule ->
          let head_pred = rule.Ast.head.predicate in
          let old_facts = U.get_facts universe ~predicate:head_pred in
          let new_facts = eval_rule universe rule in
          
          (* Only add truly new facts *)
          let delta = Relation.diff new_facts old_facts in
          
          if not (Relation.is_empty delta) then begin
            changed := true;
            U.add_derived_facts universe ~predicate:head_pred ~tuples:delta
          end
        ) rules;
        
        if !changed then iterate (iteration + 1)
        else universe
      end
    in
    
    iterate 1
  
  (* Query for matching tuples *)
  let query universe pattern =
    let pred = pattern.Ast.predicate in
    let facts = U.get_facts universe ~predicate:pred in
    Unify.match_atoms pattern facts
  
  (* Multi-atom query - join multiple atoms *)
  let multi_query universe clauses =
    (* Reuse eval_rule_body - it already does multi-atom joins! *)
    eval_rule_body universe clauses
end
