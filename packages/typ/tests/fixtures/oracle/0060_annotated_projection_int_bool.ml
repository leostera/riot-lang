(* oracle corpus fixture
   category: 01_basics
   title: annotated_projection_int_bool
   complexity: 2
   min_ocaml: 4.08
   tags: basics, annotation, tuple
*)

let project ((left, _right) : int * _) = left

let answer = project (0, true)
