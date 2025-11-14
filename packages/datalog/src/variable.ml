open Std
open Sync

type 'a t = {
  recent : 'a Relation.t Cell.t;
  stable : 'a Relation.t Cell.t;
}

let create () = {
  recent = Cell.create (Relation.empty ());
  stable = Cell.create (Relation.empty ());
}

let of_relation rel = {
  recent = Cell.create rel;
  stable = Cell.create (Relation.empty ());
}

let recent var = Cell.get var.recent
let stable var = Cell.get var.stable

let all var =
  let r = Cell.get var.recent in
  let s = Cell.get var.stable in
  Relation.merge r s

let insert var new_facts =
  let current_recent = Cell.get var.recent in
  let current_stable = Cell.get var.stable in
  
  (* Remove facts already in recent or stable *)
  let all_existing = Relation.merge current_recent current_stable in
  let truly_new = Relation.diff new_facts all_existing in
  
  (* Merge with current recent *)
  let updated_recent = Relation.merge current_recent truly_new in
  Cell.set var.recent updated_recent

let complete var =
  let r = Cell.get var.recent in
  let s = Cell.get var.stable in
  
  (* Move recent to stable *)
  let new_stable = Relation.merge s r in
  Cell.set var.stable new_stable;
  
  (* Clear recent *)
  Cell.set var.recent (Relation.empty ())

let changed var =
  let r = Cell.get var.recent in
  not (Relation.is_empty r)

let is_empty var =
  let r = Cell.get var.recent in
  let s = Cell.get var.stable in
  Relation.is_empty r && Relation.is_empty s

let size var =
  let all_facts = all var in
  Relation.length all_facts

let new_facts var candidates =
  let all_existing = all var in
  Relation.diff candidates all_existing
