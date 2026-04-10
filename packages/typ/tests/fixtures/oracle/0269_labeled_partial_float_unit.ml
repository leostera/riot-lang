(* oracle corpus fixture
   category: 07_labeled_optional
   title: labeled_partial_float_unit
   complexity: 3
   min_ocaml: 4.08
   tags: labeled_args, partial_application
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let pick_left = choose ~left:1.0 ~right:2.0

let answer = pick_left true
