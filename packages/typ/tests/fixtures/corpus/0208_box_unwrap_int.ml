(* oracle corpus fixture
   category: 05_variants
   title: box_unwrap_int
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of int

let unwrap (Box value) = value

let answer = unwrap (Box 0)
