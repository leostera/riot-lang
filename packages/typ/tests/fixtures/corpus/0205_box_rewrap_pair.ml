(* oracle corpus fixture
   category: 05_variants
   title: box_rewrap_pair
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of int * bool

let id value =
  match value with
  | Box (left, right) -> Box (left, right)

let answer = id (Box (0, true))
