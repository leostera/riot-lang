open Std

(* Occurs check: does variable appear in term? *)
let rec occurs_check ~var term =
  match term with
  | Term.Var v -> v = var
  | Term.Const _ -> false
  | Term.Wildcard -> false

(* Unify two terms *)
let rec unify_terms sub t1 t2 =
  (* Apply current substitution to both terms *)
  let t1' = Substitution.apply_to_term sub t1 in
  let t2' = Substitution.apply_to_term sub t2 in
  
  match t1', t2' with
  (* Two constants: must be equal *)
  | Term.Const v1, Term.Const v2 ->
      if Value.equal v1 v2 then Some sub else None
  
  (* Variable and constant: bind variable *)
  | Term.Var x, Term.Const v ->
      if occurs_check ~var:x t2' then None
      else Some (Substitution.bind sub ~var:x ~value:v)
  
  | Term.Const v, Term.Var x ->
      if occurs_check ~var:x t1' then None
      else Some (Substitution.bind sub ~var:x ~value:v)
  
  (* Two variables: bind first to second *)
  | Term.Var x, Term.Var y ->
      if x = y then Some sub
      else begin
        (* Bind x to y by creating a temporary constant *)
        (* Actually, we can't bind var to var directly with current Value type *)
        (* For now, keep them separate - they'll unify when bound to constants *)
        Some sub
      end
  
  (* Wildcard matches anything *)
  | Term.Wildcard, _ | _, Term.Wildcard ->
      Some sub

(* Unify two lists of terms *)
let rec unify_terms_list sub terms1 terms2 =
  match terms1, terms2 with
  | [], [] -> Some sub
  | t1 :: rest1, t2 :: rest2 ->
      (match unify_terms sub t1 t2 with
      | None -> None
      | Some sub' -> unify_terms_list sub' rest1 rest2)
  | _, _ -> None  (* Different lengths *)

(* Unify two atoms *)
let unify_atoms sub atom1 atom2 =
  (* Predicates must match *)
  if atom1.Ast.predicate = atom2.Ast.predicate then
    unify_terms_list sub atom1.Ast.args atom2.Ast.args
  else None

(* Match atom against concrete tuple *)
let match_atom sub atom tuple =
  (* Check arity *)
  let args = atom.Ast.args in
  let args_len = List.length args in
  let tuple_len = List.length tuple in
  if args_len = tuple_len then begin
    (* Try to unify each term with corresponding value *)
    let rec match_args sub args_list tuple_list =
      match args_list, tuple_list with
      | [], [] -> Some sub
      | term :: rest_args, value :: rest_tuple ->
          let value_term = Term.Const value in
          (match unify_terms sub term value_term with
          | None -> None
          | Some sub' -> match_args sub' rest_args rest_tuple)
      | _, _ -> None
    in
    match_args sub args tuple
  end else None

(* Match atom against multiple tuples *)
let match_atoms atom facts =
  let rec collect_matches acc facts_list =
    match facts_list with
    | [] -> acc
    | tuple :: rest ->
        let sub = Substitution.empty () in
        (match match_atom sub atom tuple with
        | Some sub' -> collect_matches (sub' :: acc) rest
        | None -> collect_matches acc rest)
  in
  let tuples = Relation.to_list facts in
  List.rev (collect_matches [] tuples)

(* Ground a term to a value *)
let ground sub term =
  let term' = Substitution.apply_to_term sub term in
  match term' with
  | Term.Const v -> Some v
  | Term.Var _ -> None  (* Still has unbound variable *)
  | Term.Wildcard -> None  (* Can't ground wildcard *)

(* Ground a tuple of terms *)
let ground_tuple sub terms =
  let rec ground_all acc terms_list =
    match terms_list with
    | [] -> Some (List.rev acc)
    | term :: rest ->
        (match ground sub term with
        | None -> None
        | Some value -> ground_all (value :: acc) rest)
  in
  ground_all [] terms
