(* oracle corpus fixture
   category: 04_records
   title: mutable_sequence_toggle
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, sequence
*)

type toggle = { mutable flag : bool }

let touch value =
  let before = value.flag in
  value.flag <- false;
  (before, value.flag)

let answer = touch { flag = true }
