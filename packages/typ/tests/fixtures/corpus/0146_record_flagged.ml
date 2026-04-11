(* oracle corpus fixture
   category: 03_patterns
   title: record_flagged
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, record, field
*)

type flagged = { flag : bool; code : int }

let project value =
  match value with
  | { flag; _ } -> flag

let answer = project { flag = true; code = 0 }
