(* oracle corpus fixture
   category: 14_schema_expansion
   title: extensible_variant_payload_pair_sc
   complexity: 6
   min_ocaml: 4.08
   tags: schema, extensible_variants
*)

type t = ..

type t += Payload of string * char

let id = function
  | Payload (left, right) -> Payload (left, right)

let answer = id (Payload (("x", 'y')))
