(* oracle corpus fixture
   category: 02_functions
   title: capture_option
   complexity: 2
   min_ocaml: 4.08
   tags: functions, closure, capture
*)

let seed = Some 0

let make () =
  fun value -> (seed, value)

let answer = make () ()
