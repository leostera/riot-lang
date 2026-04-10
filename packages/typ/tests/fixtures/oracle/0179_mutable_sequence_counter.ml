(* oracle corpus fixture
   category: 04_records
   title: mutable_sequence_counter
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, sequence
*)

type counter = { mutable count : int }

let touch value =
  let before = value.count in
  value.count <- 1;
  (before, value.count)

let answer = touch { count = 0 }
