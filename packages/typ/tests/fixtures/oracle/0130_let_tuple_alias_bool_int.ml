(* oracle corpus fixture
   category: 03_patterns
   title: let_tuple_alias_bool_int
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, tuple, alias, let
*)

let ((left, right) as both) = (false, 3)

let answer = (left, right, both)
