(* oracle corpus fixture
   category: 14_schema_expansion
   title: optional_default_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, optional_args
*)

type 'a option =
  | Some of 'a
  | None

let build ?(value = (Some 'x')) () = value

let answer = build ()
