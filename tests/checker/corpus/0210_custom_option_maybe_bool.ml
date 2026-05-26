(* oracle corpus fixture
   category: 05_variants
   title: custom_option_maybe_bool
   complexity: 2
   min_ocaml: 4.08
   tags: variants, custom_option
*)

type 'a maybe = Nothing | Just of 'a

let map_just f value =
  match value with
  | Nothing -> Nothing
  | Just x -> Just (f x)

let answer = map_just (fun x -> x) (Just true)
