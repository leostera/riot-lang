(* oracle corpus fixture
   category: 02_functions
   title: flip_pair_string
   complexity: 2
   min_ocaml: 4.08
   tags: functions, flip, higher_order
*)

let flip f x y = f y x

let pair x y = (x, y)

let answer = flip pair () "word"
