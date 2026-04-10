(* oracle corpus fixture
   category: 05_variants
   title: binary_variant_either_string_char
   complexity: 2
   min_ocaml: 4.08
   tags: variants, binary_variant, match
*)

type ('a, 'b) either = Left of 'a | Right of 'b

let project value =
  match value with
  | Left x -> x
  | Right _ -> ""

let answer = project (Left "x")
