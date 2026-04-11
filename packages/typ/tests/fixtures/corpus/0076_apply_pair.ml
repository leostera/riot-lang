(* oracle corpus fixture
   category: 02_functions
   title: apply_pair
   complexity: 1
   min_ocaml: 4.08
   tags: functions, apply, higher_order
*)

let apply f x = f x

let id x = x

let answer = apply id (0, true)
