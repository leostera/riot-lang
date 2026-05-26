(* oracle corpus fixture
   category: 14_schema_expansion
   title: extensible_variant_payload_list_int
   complexity: 6
   min_ocaml: 4.08
   tags: schema, extensible_variants
*)

type t = ..

type t += Payload of int list

let id = function
  | Payload x -> Payload x

let answer = id (Payload ([0; 1]))
