(* oracle corpus fixture
   category: 05_variants
   title: box_unwrap_pair
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of int * bool

let unwrap (Box (left, right)) = (left, right)

let answer = unwrap (Box (0, true))
