(* oracle corpus fixture
   category: 14_schema_expansion
   title: custom_maybe_option_char
   complexity: 3
   min_ocaml: 4.08
   tags: schema, variant, custom_option
*)

type maybe = Nothing | Just of char option

let id = function
  | Nothing -> Nothing
  | Just x -> Just x

let answer = id (Just (Some 'x'))
