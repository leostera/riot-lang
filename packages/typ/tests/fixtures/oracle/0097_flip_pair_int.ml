(* oracle corpus fixture
   category: 02_functions
   title: flip_pair_int
   complexity: 2
   min_ocaml: 4.08
   tags: functions, flip, higher_order
*)

let flip f x y = f y x

let pair x y = (x, y)

let answer = flip pair () 0
