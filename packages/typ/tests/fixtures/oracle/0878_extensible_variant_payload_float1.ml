(* oracle corpus fixture
   category: 14_schema_expansion
   title: extensible_variant_payload_float1
   complexity: 6
   min_ocaml: 4.08
   tags: schema, extensible_variants
*)

type t = ..

type t += Payload of float

let id = function
  | Payload x -> Payload x

let answer = id (Payload (1.5))
