(* oracle corpus fixture
   category: 02_functions
   title: compose_string_char
   complexity: 2
   min_ocaml: 4.08
   tags: functions, compose, higher_order
*)

let to_pair x = (x, 'x')
let first (x, _c) = x

        let compose f g x = f (g x)

        let answer = compose first to_pair "typ"
