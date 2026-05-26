(* oracle corpus fixture
   category: 07_labeled_optional
   title: higher_order_labeled_char
   complexity: 4
   min_ocaml: 4.08
   tags: labeled_args, higher_order
*)

let use chooser = chooser ~left:'x' ~right:'x' true

let choose ~left ~right flag =
  if flag then
    left
  else
    right

let answer = use choose
