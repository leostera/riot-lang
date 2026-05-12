(* oracle corpus fixture
   category: 02_functions
   title: curry_curry_int_bool
   complexity: 2
   min_ocaml: 4.08
   tags: functions, curry, tuple
*)

let uncurry f (x, y) = f x y

let pair x y = (x, y)

let answer = uncurry pair (0, true)
