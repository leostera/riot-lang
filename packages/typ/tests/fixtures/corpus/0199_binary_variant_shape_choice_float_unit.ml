(* oracle corpus fixture
   category: 05_variants
   title: binary_variant_shape_choice_float_unit
   complexity: 2
   min_ocaml: 4.08
   tags: variants, binary_variant, constructor_shape
*)

type ('a, 'b) choice = This of 'a | That of 'b

let keep value =
  match value with
  | This x -> This x
  | That y -> That y

let answer = keep (That ())
