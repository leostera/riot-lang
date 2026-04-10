(* oracle corpus fixture
   category: 04_records
   title: mutable_sequence_slot
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, sequence
*)

type 'a slot = { mutable value : 'a }

let touch value =
  let before = value.value in
  value.value <- 'y';
  (before, value.value)

let answer = touch { value = 'x' }
