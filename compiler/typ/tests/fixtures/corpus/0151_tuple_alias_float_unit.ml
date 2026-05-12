(* oracle corpus fixture
   category: 03_patterns
   title: tuple_alias_float_unit
   complexity: 2
   min_ocaml: 4.08
   tags: patterns, tuple, alias
*)

let describe pair =
  match pair with
  | ((left, right) as both) -> (left, right, both)

let answer = describe (1.0, ())
