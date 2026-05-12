(* oracle corpus fixture
   category: 05_variants
   title: enum_function_shape
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum, function
*)

type shape = Dot | Line | Area

let choose = function
  | Dot -> Dot
  | Line -> Line
  | Area -> Area

let answer = choose Dot
