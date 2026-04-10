(* oracle corpus fixture
   category: 04_records
   title: functional_update_point
   complexity: 3
   min_ocaml: 4.08
   tags: records, functional_update
*)

type point = { x : int; y : int }

let bump value =
  { value with x = 2 }

let answer = bump { x = 0; y = 1 }
