(* oracle corpus fixture
   category: 14_schema_expansion
   title: duplicate_pair_bool_t
   complexity: 2
   min_ocaml: 4.08
   tags: schema, functions, pair
*)

let duplicate x = (x, x)

let answer = duplicate (true)
