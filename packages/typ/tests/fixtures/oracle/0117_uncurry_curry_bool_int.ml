(* oracle corpus fixture
   category: 02_functions
   title: uncurry_curry_bool_int
   complexity: 2
   min_ocaml: 4.08
   tags: functions, uncurry, tuple
*)

let curry f x y = f (x, y)

let pair (x, y) = (x, y)

let answer = curry pair false 2
