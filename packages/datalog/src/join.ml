open Std
open Collections

type join_result = {
  substitution : Substitution.t;
  tuple : Storage.fact_tuple;
}

(* Extract variable names from a term *)
let rec term_vars acc term =
  match term with
  | Term.Var v -> if List.mem v acc then acc else v :: acc
  | Term.Const _ | Term.Wildcard -> acc

(* Get all variables from an atom *)
let atom_vars atom =
  List.fold_left term_vars [] atom.Ast.args
  |> List.rev

(* Find shared variables between two atoms *)
let shared_vars atom1 atom2 =
  let vars1 = atom_vars atom1 in
  let vars2 = atom_vars atom2 in
  List.filter (fun v -> List.mem v vars2) vars1

(* Project substitution onto specific variables *)
let project ~vars sub =
  let rec collect_values acc remaining_vars =
    match remaining_vars with
    | [] -> Some (List.rev acc)
    | var :: rest ->
        (match Substitution.lookup sub ~var with
        | Some value -> collect_values (value :: acc) rest
        | None -> None)
  in
  collect_values [] vars

(* Cartesian product of two relations *)
let cartesian_product rel1 rel2 =
  let tuples1 = Relation.to_list rel1 in
  let tuples2 = Relation.to_list rel2 in
  let rec make_pairs acc list1 list2 =
    match list1 with
    | [] -> List.rev acc
    | t1 :: rest1 ->
        let pairs_for_t1 = List.map (fun t2 -> (t1, t2)) list2 in
        make_pairs (List.rev_append pairs_for_t1 acc) rest1 list2
  in
  make_pairs [] tuples1 tuples2

(* Join two relations on shared variables *)
let join_atoms atom1 rel1 atom2 rel2 =
  (* Get all tuples *)
  let tuples1 = Relation.to_list rel1 in
  let tuples2 = Relation.to_list rel2 in
  
  (* For each tuple in rel1, try to join with each tuple in rel2 *)
  let rec try_joins acc remaining1 =
    match remaining1 with
    | [] -> List.rev acc
    | tuple1 :: rest1 ->
        (* Match atom1 with tuple1 to get substitution *)
        let sub1 = Substitution.empty () in
        (match Unify.match_atom sub1 atom1 tuple1 with
        | None -> try_joins acc rest1
        | Some sub1' ->
            (* Try to extend sub1' with each tuple2 *)
            let rec try_tuple2 acc2 remaining2 =
              match remaining2 with
              | [] -> acc2
              | tuple2 :: rest2 ->
                  (* Try to match atom2 with tuple2, extending sub1' *)
                  (match Unify.match_atom sub1' atom2 tuple2 with
                  | None -> try_tuple2 acc2 rest2
                  | Some sub_combined ->
                      (* Success! Extract all variables for result tuple *)
                      let all_vars = 
                        let v1 = atom_vars atom1 in
                        let v2 = atom_vars atom2 in
                        (* Combine and deduplicate *)
                        let rec add_unique acc vars =
                          match vars with
                          | [] -> acc
                          | v :: rest ->
                              if List.mem v acc then add_unique acc rest
                              else add_unique (v :: acc) rest
                        in
                        List.rev (add_unique v1 v2)
                      in
                      
                      (match project ~vars:all_vars sub_combined with
                      | None -> try_tuple2 acc2 rest2
                      | Some result_tuple ->
                          let result = {
                            substitution = sub_combined;
                            tuple = result_tuple;
                          } in
                          try_tuple2 (result :: acc2) rest2))
            in
            let new_results = try_tuple2 [] tuples2 in
            try_joins (List.rev_append new_results acc) rest1)
  in
  try_joins [] tuples1
