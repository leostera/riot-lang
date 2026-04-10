(* oracle corpus fixture
   category: 14_schema_expansion
   title: custom_maybe_pair_ib
   complexity: 3
   min_ocaml: 4.08
   tags: schema, variant, custom_option
*)

type maybe = Nothing | Just of int * bool

let id = function
  | Nothing -> Nothing
  | Just (left, right) -> Just (left, right)

let answer = id (Just ((0, true)))
