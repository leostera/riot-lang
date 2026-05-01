(* oracle corpus fixture
   category: 02_functions
   title: const_returning_function_option_int
   complexity: 2
   min_ocaml: 4.08
   tags: functions, const, higher_order, partial_application
*)

type 'a option =
  | Some of 'a
  | None

let const x _ = x

let keep = const (fun value -> value)

let answer = keep () (Some 0)
