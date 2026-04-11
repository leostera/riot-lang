(* oracle corpus fixture
   category: 07_labeled_optional
   title: labeled_partial_string_char
   complexity: 3
   min_ocaml: 4.08
   tags: labeled_args, partial_application
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let pick_left = choose ~left:"x" ~right:"y"

let answer = pick_left true
