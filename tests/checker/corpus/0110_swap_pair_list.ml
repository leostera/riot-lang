(* oracle corpus fixture
   category: 02_functions
   title: swap_pair_list
   complexity: 2
   min_ocaml: 4.08
   tags: functions, tuple, polymorphic
*)

let swap (left, right) = (right, left)

let answer = swap ([0], ())
