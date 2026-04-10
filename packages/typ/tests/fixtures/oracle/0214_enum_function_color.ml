(* oracle corpus fixture
   category: 05_variants
   title: enum_function_color
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum, function
*)

type color = Red | Blue | Green

let choose = function
  | Red -> Red
  | Blue -> Blue
  | Green -> Green

let answer = choose Red
