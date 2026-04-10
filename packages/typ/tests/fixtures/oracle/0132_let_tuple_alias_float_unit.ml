(* oracle corpus fixture
   category: 03_patterns
   title: let_tuple_alias_float_unit
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, tuple, alias, let
*)

let ((left, right) as both) = (1.0, ())

let answer = (left, right, both)
