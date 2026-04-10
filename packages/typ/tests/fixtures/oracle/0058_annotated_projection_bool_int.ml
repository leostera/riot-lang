(* oracle corpus fixture
   category: 01_basics
   title: annotated_projection_bool_int
   complexity: 2
   min_ocaml: 4.08
   tags: basics, annotation, tuple
*)

let project ((left, _right) : bool * _) = left

let answer = project (false, 1)
