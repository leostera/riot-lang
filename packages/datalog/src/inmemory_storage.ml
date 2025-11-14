open Std
open Collections

type t = (string, Storage.fact_tuple Relation.t) HashMap.t

let create () = HashMap.create ()

let add_fact storage ~predicate ~tuple =
  let current = match HashMap.get storage predicate with
    | Some rel -> rel
    | None -> Relation.empty ()
  in
  let new_rel = Relation.merge current (Relation.singleton tuple) in
  HashMap.insert storage predicate new_rel |> ignore

let add_facts storage ~predicate ~tuples =
  let current = match HashMap.get storage predicate with
    | Some rel -> rel
    | None -> Relation.empty ()
  in
  let new_rel = Relation.merge current (Relation.of_list tuples) in
  HashMap.insert storage predicate new_rel |> ignore

let of_facts facts_list =
  let storage = create () in
  List.iter (fun (predicate, tuples) ->
    add_facts storage ~predicate ~tuples
  ) facts_list;
  storage

let get_facts storage ~predicate =
  match HashMap.get storage predicate with
  | Some rel -> rel
  | None -> Relation.empty ()

let predicates storage =
  let preds = Vector.create () in
  HashMap.iter (fun pred _rel -> Vector.push preds pred) storage;
  (* Convert vector to list *)
  let rec vec_to_list acc i =
    match Vector.get preds i with
    | Some x -> vec_to_list (x :: acc) (i + 1)
    | None -> List.rev acc
  in
  vec_to_list [] 0

let iter_facts storage ~predicate f =
  match HashMap.get storage predicate with
  | Some rel -> Relation.iter f rel
  | None -> ()

let get_facts_matching storage ~predicate ~pattern =
  let facts = get_facts storage ~predicate in
  Relation.filter (Storage.matches_pattern pattern) facts

let clear storage ~predicate =
  HashMap.remove storage predicate |> ignore

let clear_all storage =
  HashMap.clear storage

let fact_count storage ~predicate =
  match HashMap.get storage predicate with
  | Some rel -> Relation.length rel
  | None -> 0

let total_facts storage =
  let total = ref 0 in
  HashMap.iter (fun _pred rel -> 
    total := !total + Relation.length rel
  ) storage;
  !total
