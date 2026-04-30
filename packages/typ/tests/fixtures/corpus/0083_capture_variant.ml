(* oracle corpus fixture
   category: 02_functions
   title: capture_variant
   complexity: 2
   min_ocaml: 4.08
   tags: functions, closure, capture
*)

type color =
  | Red
  | Blue

let seed = Red

let make () = fun value -> (seed, value)

let answer = make () ()
