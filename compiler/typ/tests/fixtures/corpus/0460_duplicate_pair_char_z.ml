(* oracle corpus fixture
   category: 14_schema_expansion
   title: duplicate_pair_char_z
   complexity: 2
   min_ocaml: 4.08
   tags: schema, functions, pair
*)

let duplicate x = (x, x)

let answer = duplicate ('z')
