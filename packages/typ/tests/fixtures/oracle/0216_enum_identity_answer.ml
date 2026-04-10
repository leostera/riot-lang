(* oracle corpus fixture
   category: 05_variants
   title: enum_identity_answer
   complexity: 2
   min_ocaml: 4.08
   tags: variants, enum
*)

type answer = Yes | No | Maybe

let id value =
  match value with
  | Yes -> Yes
  | No -> No
  | Maybe -> Maybe

let answer = id Yes
