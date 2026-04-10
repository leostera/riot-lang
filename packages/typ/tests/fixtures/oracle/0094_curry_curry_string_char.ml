(* oracle corpus fixture
   category: 02_functions
   title: curry_curry_string_char
   complexity: 2
   min_ocaml: 4.08
   tags: functions, curry, tuple
*)

let uncurry f (x, y) = f x y

let pair x y = (x, y)

let answer = uncurry pair ("x", 'y')
