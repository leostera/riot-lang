(* oracle corpus fixture
   category: 03_patterns
   title: function_choice
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, function, variant
*)

type ('a, 'b) choice = This of 'a | That of 'b

let unwrap = function
  | This x -> This x
  | That y -> That y

let answer = unwrap (This "x")
