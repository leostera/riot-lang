(* oracle corpus fixture
   category: 14_schema_expansion
   title: closure_capture_pair_ib
   complexity: 3
   min_ocaml: 4.08
   tags: schema, functions, closure
*)

let make seed =
  fun value -> (seed, value)

let answer = make ((0, true)) ((1, false))
