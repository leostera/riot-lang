(* oracle corpus fixture
   category: 14_schema_expansion
   title: duplicate_pair_option_char
   complexity: 2
   min_ocaml: 4.08
   tags: schema, functions, pair
*)

type 'a option =
  | Some of 'a
  | None

let duplicate x = (x, x)

let answer = duplicate (Some 'x')
