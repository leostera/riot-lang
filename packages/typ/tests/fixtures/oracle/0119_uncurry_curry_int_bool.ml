(* oracle corpus fixture
   category: 02_functions
   title: uncurry_curry_int_bool
   complexity: 2
   min_ocaml: 4.08
   tags: functions, uncurry, tuple
*)

let curry f x y = f (x, y)

let pair (x, y) = (x, y)

let answer = curry pair 0 true
