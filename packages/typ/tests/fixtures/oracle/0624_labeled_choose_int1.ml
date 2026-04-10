(* oracle corpus fixture
   category: 14_schema_expansion
   title: labeled_choose_int1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, labeled_args
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let answer = choose ~left:(1) ~right:(2) true
