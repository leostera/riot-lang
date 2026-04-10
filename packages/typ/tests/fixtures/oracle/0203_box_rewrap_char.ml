(* oracle corpus fixture
   category: 05_variants
   title: box_rewrap_char
   complexity: 2
   min_ocaml: 4.08
   tags: variants, constructor, payload
*)

type box = Box of char

let id value =
  match value with
  | Box inner -> Box inner

let answer = id (Box 'x')
