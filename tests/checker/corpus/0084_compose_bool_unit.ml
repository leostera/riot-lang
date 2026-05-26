(* oracle corpus fixture
   category: 02_functions
   title: compose_bool_unit
   complexity: 2
   min_ocaml: 4.08
   tags: functions, compose, higher_order
*)

let to_pair x = (x, ())
let first (x, _u) = x

        let compose f g x = f (g x)

        let answer = compose first to_pair true
