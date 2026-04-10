(* oracle corpus fixture
   category: 01_basics
   title: annotated_projection_float_unit
   complexity: 2
   min_ocaml: 4.08
   tags: basics, annotation, tuple
*)

let project ((left, _right) : float * _) = left

let answer = project (1.0, ())
