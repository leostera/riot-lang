(* oracle corpus fixture
   category: 03_patterns
   title: function_either
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, function, variant
*)

type ('a, 'b) either = Left of 'a | Right of 'b

let unwrap = function
  | Left x -> Left x
  | Right y -> Right y

let answer = unwrap (Left 0)
