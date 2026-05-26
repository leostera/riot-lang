(* oracle corpus fixture
   category: 07_labeled_optional
   title: labeled_choose_int_bool
   complexity: 3
   min_ocaml: 4.08
   tags: labeled_args, optional_args, functions
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let answer = choose ~right:1 ~left:0 true
