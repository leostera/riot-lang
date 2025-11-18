open Std
open Collections

(** Simple in-memory storage for testing Phase 0 query-only Datalog *)

type t = {
  facts : (string, Storage.fact_tuple list) HashMap.t;
}

let create () = {
  facts = HashMap.create ();
}

let add_fact storage ~predicate ~tuple =
  let current = match HashMap.get storage.facts predicate with
    | Some tuples -> tuples
    | None -> []
  in
  HashMap.insert storage.facts predicate (tuple :: current) |> ignore

let get_facts_matching storage ~predicate ~pattern =
  let tuples = match HashMap.get storage.facts predicate with
    | Some tuples -> tuples
    | None -> []
  in
  
  (* Filter by pattern *)
  let filtered = List.filter (Storage.matches_pattern pattern) tuples in
  
  (* Return as relation (sorted and deduplicated) *)
  Relation.of_list filtered

let of_facts facts_list =
  let storage = create () in
  List.iter (fun (predicate, tuples) ->
    List.iter (fun tuple ->
      add_fact storage ~predicate ~tuple
    ) tuples
  ) facts_list;
  storage
