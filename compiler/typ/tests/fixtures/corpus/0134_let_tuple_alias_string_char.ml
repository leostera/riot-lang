(* oracle corpus fixture
   category: 03_patterns
   title: let_tuple_alias_string_char
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, tuple, alias, let
*)

let ((left, right) as both) = ("name", 'n')

let answer = (left, right, both)
