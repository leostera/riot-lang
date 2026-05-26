(* oracle corpus fixture
   category: 14_schema_expansion
   title: polyvariant_payload_pair_sc
   complexity: 5
   min_ocaml: 4.08
   tags: schema, polyvariant, payload
*)

let id = function
  | `Payload x -> `Payload x

let answer = id (`Payload (("x", 'y')))
