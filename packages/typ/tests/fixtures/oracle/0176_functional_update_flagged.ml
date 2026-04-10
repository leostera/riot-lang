(* oracle corpus fixture
   category: 04_records
   title: functional_update_flagged
   complexity: 3
   min_ocaml: 4.08
   tags: records, functional_update
*)

type flagged = { flag : bool; code : int }

let bump value =
  { value with code = 1 }

let answer = bump { flag = true; code = 0 }
