(* oracle corpus fixture
   category: 05_variants
   title: binary_variant_shape_either_int_bool
   complexity: 2
   min_ocaml: 4.08
   tags: variants, binary_variant, constructor_shape
*)

type ('a, 'b) either = Left of 'a | Right of 'b

let keep value =
  match value with
  | Left x -> Left x
  | Right y -> Right y

let answer = keep (Right true)
