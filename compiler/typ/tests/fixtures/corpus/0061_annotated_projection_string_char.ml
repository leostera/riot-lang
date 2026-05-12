(* oracle corpus fixture
   category: 01_basics
   title: annotated_projection_string_char
   complexity: 2
   min_ocaml: 4.08
   tags: basics, annotation, tuple
*)

let project ((left, _right) : string * _) = left

let answer = project ("x", 'y')
