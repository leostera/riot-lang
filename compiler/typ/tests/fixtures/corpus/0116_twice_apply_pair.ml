(* oracle corpus fixture
   category: 02_functions
   title: twice_apply_pair
   complexity: 2
   min_ocaml: 4.08
   tags: functions, higher_order, twice
*)

let twice f x = f (f x)

let id x = x

let answer = twice id (0, true)
