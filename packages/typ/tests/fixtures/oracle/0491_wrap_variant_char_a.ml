(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_char_a
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of char

let unwrap (Wrap value) = value

let answer = unwrap (Wrap ('a'))
