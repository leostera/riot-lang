(* oracle corpus fixture
   category: 05_variants
   title: enum_identity_color
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum
*)

type color = Red | Blue | Green

let id value =
  match value with
  | Red -> Red
  | Blue -> Blue
  | Green -> Green

let answer = id Red
