open Std
open Collections

module Make (S : Storage.STORAGE) = struct
  type t = {
    storage : S.t;
    derived : (string, Storage.fact_tuple Relation.t) HashMap.t;
    rules : Ast.rule Vector.t;
  }
  
  let create storage = {
    storage;
    derived = HashMap.create ();
    rules = Vector.create ();
  }
  
  (* Rules *)
  
  let add_rule universe rule =
    Vector.push universe.rules rule;
    universe
  
  let add_rules universe rules_list =
    List.iter (fun rule -> Vector.push universe.rules rule) rules_list;
    universe
  
  let rules universe =
    (* Convert vector to list *)
    let rec vec_to_list acc i =
      match Vector.get universe.rules i with
      | Some x -> vec_to_list (x :: acc) (i + 1)
      | None -> List.rev acc
    in
    vec_to_list [] 0
  
  (* Derived facts *)
  
  let add_derived_fact universe ~predicate ~tuple =
    let current = match HashMap.get universe.derived predicate with
      | Some rel -> rel
      | None -> Relation.empty ()
    in
    let new_rel = Relation.merge current (Relation.singleton tuple) in
    HashMap.insert universe.derived predicate new_rel |> ignore
  
  let add_derived_facts universe ~predicate ~tuples =
    let current = match HashMap.get universe.derived predicate with
      | Some rel -> rel
      | None -> Relation.empty ()
    in
    let new_rel = Relation.merge current tuples in
    HashMap.insert universe.derived predicate new_rel |> ignore
  
  let clear_derived universe =
    HashMap.clear universe.derived
  
  (* Fact access *)
  
  let get_base_facts universe ~predicate =
    S.get_facts universe.storage ~predicate
  
  let get_derived_facts universe ~predicate =
    match HashMap.get universe.derived predicate with
    | Some rel -> rel
    | None -> Relation.empty ()
  
  let get_facts universe ~predicate =
    let base = get_base_facts universe ~predicate in
    let derived = get_derived_facts universe ~predicate in
    Relation.merge base derived
  
  let contains_fact universe ~predicate ~tuple =
    let facts = get_facts universe ~predicate in
    Relation.contains facts tuple
  
  (* Introspection *)
  
  let base_predicates universe =
    S.predicates universe.storage
  
  let derived_predicates universe =
    let preds = Vector.create () in
    HashMap.iter (fun pred _rel -> Vector.push preds pred) universe.derived;
    (* Convert vector to list *)
    let rec vec_to_list acc i =
      match Vector.get preds i with
      | Some x -> vec_to_list (x :: acc) (i + 1)
      | None -> List.rev acc
    in
    vec_to_list [] 0
  
  let predicates universe =
    let base = base_predicates universe in
    let derived = derived_predicates universe in
    (* Deduplicate by converting to set-like structure *)
    let all = base @ derived in
    List.sort_uniq String.compare all
  
  let storage universe = universe.storage
end

(* Default InMemory universe *)

module InMemory = struct
  include Make(Inmemory_storage)
  
  let create_empty () =
    create (Inmemory_storage.create ())
  
  let of_facts facts_list =
    create (Inmemory_storage.of_facts facts_list)
end
