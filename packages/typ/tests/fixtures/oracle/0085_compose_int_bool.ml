(* oracle corpus fixture
   category: 02_functions
   title: compose_int_bool
   complexity: 2
   min_ocaml: 4.08
   tags: functions, compose, higher_order
*)

let to_pair x = (x, true)
let first (x, _flag) = x

        let compose f g x = f (g x)

        let answer = compose first to_pair 0
