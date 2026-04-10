(* oracle corpus fixture
   category: 02_functions
   title: capture_record
   complexity: 2
   min_ocaml: 4.08
   tags: functions, closure, capture
*)

type box = { value : int }

let seed = { value = 0 }

let make () =
  fun value -> (seed, value)

let answer = make () ()
