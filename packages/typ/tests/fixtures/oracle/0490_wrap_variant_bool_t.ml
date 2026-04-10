(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_bool_t
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of bool

let unwrap (Wrap value) = value

let answer = unwrap (Wrap (true))
