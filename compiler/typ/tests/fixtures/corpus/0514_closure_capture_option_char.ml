(* oracle corpus fixture
   category: 14_schema_expansion
   title: closure_capture_option_char
   complexity: 3
   min_ocaml: 4.08
   tags: schema, functions, closure
*)

type 'a option =
  | Some of 'a
  | None

let make seed =
  fun value -> (seed, value)

let answer = make (Some 'x') (None)
