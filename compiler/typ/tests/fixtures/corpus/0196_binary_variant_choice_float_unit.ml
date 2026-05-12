(* oracle corpus fixture
   category: 05_variants
   title: binary_variant_choice_float_unit
   complexity: 2
   min_ocaml: 4.08
   tags: variants, binary_variant, match
*)

type ('a, 'b) choice = This of 'a | That of 'b

let project value =
  match value with
  | This x -> x
  | That _ -> 0.0

let answer = project (This 0.0)
