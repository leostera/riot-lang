(* oracle corpus fixture
   category: 05_variants
   title: box_unwrap_bool
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of bool

let unwrap (Box value) = value

let answer = unwrap (Box true)
