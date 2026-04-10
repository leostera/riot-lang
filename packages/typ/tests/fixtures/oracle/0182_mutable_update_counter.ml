(* oracle corpus fixture
   category: 04_records
   title: mutable_update_counter
   complexity: 3
   min_ocaml: 4.08
   tags: records, mutable, update
*)

type counter = { mutable count : int }

let set value =
  value.count <- 1;
  value

let answer = set { count = 0 }
