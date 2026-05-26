(* oracle corpus fixture
   category: 14_schema_expansion
   title: labeled_choose_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, labeled_args
*)

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let answer = choose ~left:(("x", 'y')) ~right:(("z", 'w')) true
