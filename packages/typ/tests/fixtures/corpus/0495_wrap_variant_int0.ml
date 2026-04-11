(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_int0
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of int

let unwrap (Wrap value) = value

let answer = unwrap (Wrap (0))
