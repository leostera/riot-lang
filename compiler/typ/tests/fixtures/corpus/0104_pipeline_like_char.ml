(* oracle corpus fixture
   category: 02_functions
   title: pipeline_like_char
   complexity: 2
   min_ocaml: 4.08
   tags: functions, higher_order, application
*)

let apply x f = f x

let id x = x

let answer = apply 'z' id
