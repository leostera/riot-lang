(* oracle corpus fixture
   category: 13_modules
   title: nested_module_constructors_do_not_leak_unqualified
   complexity: 4
   min_ocaml: 4.08
   tags: modules, constructors, scope, patterns
*)

module Nested = struct
  type t =
    | File of bool
    | System of char
end

type error =
  | File of string
  | System of int

let to_string = fun value ->
  match value with
  | File error -> error
  | System _ -> "system"

let answer = to_string (File "ok")
