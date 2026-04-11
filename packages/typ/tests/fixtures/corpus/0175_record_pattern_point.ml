(* oracle corpus fixture
   category: 04_records
   title: record_pattern_point
   complexity: 2
   min_ocaml: 4.08
   tags: records, pattern, field
*)

type point = { x : int; y : int }

let get value =
  let { x; _ } = value in
  x

let answer = get { x = 0; y = 1 }
