(* oracle corpus fixture
   category: 02_functions
   title: const_returning_function_list_int
   complexity: 2
   min_ocaml: 4.08
   tags: functions, const, higher_order, partial_application
*)

let const x _ = x

let keep = const (fun value -> value)

let answer = keep () [0; 1]
