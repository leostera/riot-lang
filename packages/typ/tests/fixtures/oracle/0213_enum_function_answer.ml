(* oracle corpus fixture
   category: 05_variants
   title: enum_function_answer
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum, function
*)

type answer = Yes | No | Maybe

let choose = function
  | Yes -> Yes
  | No -> No
  | Maybe -> Maybe

let answer = choose Yes
