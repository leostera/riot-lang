(* oracle corpus fixture
   category: 05_variants
   title: box_unwrap_char
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of char

let unwrap (Box value) = value

let answer = unwrap (Box 'x')
