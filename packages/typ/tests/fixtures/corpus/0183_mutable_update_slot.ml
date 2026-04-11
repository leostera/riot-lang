(* oracle corpus fixture
   category: 04_records
   title: mutable_update_slot
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, update
*)

type 'a slot = { mutable value : 'a }

let set value =
  value.value <- 'y';
  value

let answer = set { value = 'x' }
