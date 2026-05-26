(* oracle corpus fixture
   category: 14_schema_expansion
   title: closure_capture_int1
   complexity: 3
   min_ocaml: 4.08
   tags: schema, functions, closure
*)

let make seed =
  fun value -> (seed, value)

let answer = make (1) (2)
