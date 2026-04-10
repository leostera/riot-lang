(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_string_typ
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of string

let unwrap (Wrap value) = value

let answer = unwrap (Wrap ("typ"))
