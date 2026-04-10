(* oracle corpus fixture
   category: 02_functions
   title: make_left_int
   complexity: 2
   min_ocaml: 4.08
   tags: functions, closure, returning_function
*)

let make_left left =
  fun right -> (left, right)

let answer = make_left 0 true
