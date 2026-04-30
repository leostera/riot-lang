(* oracle corpus fixture
   category: 03_patterns
   title: record_point
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, record, field
*)

type point = { x: int; y: int }

let project value =
  match value with
  | { x; _ } -> x

let answer = project { x = 0; y = 1 }
