(* oracle corpus fixture
   category: 02_functions
   title: swap_pair_bool
   complexity: 2
   min_ocaml: 4.08
   tags: functions, tuple, polymorphic
*)

let swap (left, right) = (right, left)

let answer = swap (true, ())
