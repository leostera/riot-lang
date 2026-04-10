(* oracle corpus fixture
   category: 01_basics
   title: choose_string
   complexity: 1
   min_ocaml: 4.08
   tags: basics, if, branching
*)

let choose flag left right =
  if flag then
    left
  else
    right

let answer = choose true "left" "right"
