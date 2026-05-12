(* oracle corpus fixture
   category: 14_schema_expansion
   title: wrap_variant_pair_sc
   complexity: 2
   min_ocaml: 4.08
   tags: schema, variant, wrapper
*)

type t = Wrap of string * char

let unwrap (Wrap (left, right)) = (left, right)

let answer = unwrap (Wrap (("x", 'y')))
