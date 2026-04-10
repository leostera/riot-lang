(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_option_int
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of int option

let unwrap (Wrap value) = value

let answer = unwrap (Wrap (Some 0))
