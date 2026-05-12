(* oracle corpus fixture
   category: 03_patterns
   title: function_resultish
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, function, variant
*)

type ('a, 'b) resultish = Ok of 'a | Error of 'b

let unwrap = function
  | Ok x -> Ok x
  | Error y -> Error y

let answer = unwrap (Ok 0.0)
