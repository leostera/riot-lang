(* oracle corpus fixture
   category: 01_basics
   title: let_pattern_float_unit
   complexity: 1
   min_ocaml: 4.08
   tags: basics, tuple, pattern, let
*)

let pair = (1.0, ())

let left, right = pair

let answer = left
