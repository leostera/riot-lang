(* oracle corpus fixture
   category: 04_records
   title: mutable_update_toggle
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, update
*)

type toggle = { mutable flag : bool }

let set value =
  value.flag <- false;
  value

let answer = set { flag = true }
