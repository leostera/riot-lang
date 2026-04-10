(* oracle corpus fixture
   category: 14_schema_expansion
   title: closure_capture_pair_sc
   complexity: 3
   min_ocaml: 4.08
   tags: schema, functions, closure
*)

let make seed =
  fun value -> (seed, value)

let answer = make (("x", 'y')) (("z", 'w'))
