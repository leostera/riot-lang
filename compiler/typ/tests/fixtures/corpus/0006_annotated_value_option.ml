(* oracle corpus fixture
   category: 01_basics
   title: annotated_value_option
   complexity: 1
   min_ocaml: 4.08
   tags: basics, annotation, value
*)

type 'a option =
  | Some of 'a
  | None

let value : char option = Some 'x'

let answer = value
