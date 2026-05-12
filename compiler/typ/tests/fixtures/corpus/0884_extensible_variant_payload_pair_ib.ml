(* oracle corpus fixture
   category: 14_schema_expansion
   title: extensible_variant_payload_pair_ib
   complexity: 6
   min_ocaml: 4.08
   tags: schema, extensible_variants
*)

type t = ..

type t += Payload of int * bool

let id = function
  | Payload (left, right) -> Payload (left, right)

let answer = id (Payload ((0, true)))
