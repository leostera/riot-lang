(* oracle corpus fixture
   category: 05_variants
   title: enum_identity_shape
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum
*)

type shape =
  | Dot
  | Line
  | Area

let id value =
  match value with
  | Dot -> Dot
  | Line -> Line
  | Area -> Area

let answer = id Dot
