(* oracle corpus fixture
   category: 14_schema_expansion
   title: labeled_partial_positional_after_label
   complexity: 4
   min_ocaml: 4.08
   tags: schema, labeled_args, partial_application
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let pick_left = choose ~left:0

let answer = pick_left true
