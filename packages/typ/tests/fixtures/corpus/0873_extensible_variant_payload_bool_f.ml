(* oracle corpus fixture
   category: 14_schema_expansion
   title: extensible_variant_payload_bool_f
   complexity: 6
   min_ocaml: 4.08
   tags: schema, extensible_variants
*)

type t = ..

type t += Payload of bool

let id = function
  | Payload x -> Payload x

let answer = id (Payload (false))
