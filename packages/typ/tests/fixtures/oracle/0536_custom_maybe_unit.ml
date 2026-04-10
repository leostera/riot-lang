(* oracle corpus fixture
   category: 14_schema_expansion
   title: custom_maybe_unit
   complexity: 3
   min_ocaml: 4.08
   tags: schema, variant, custom_option
*)

type maybe = Nothing | Just of unit

let id = function
  | Nothing -> Nothing
  | Just x -> Just x

let answer = id (Just (()))
