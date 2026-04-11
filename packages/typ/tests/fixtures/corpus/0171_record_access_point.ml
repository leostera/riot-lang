(* oracle corpus fixture
   category: 04_records
   title: record_access_point
   complexity: 2
   min_ocaml: 4.08
   tags: records, access, field
*)

type point = { x : int; y : int }

let get value = value.x

let answer = get { x = 0; y = 1 }
