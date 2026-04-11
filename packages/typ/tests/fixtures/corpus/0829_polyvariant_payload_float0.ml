(* oracle corpus fixture
   category: 14_schema_expansion
   title: polyvariant_payload_float0
   complexity: 5
   min_ocaml: 4.08
   tags: schema, polyvariant, payload
*)

let id = function
  | `Payload x -> `Payload x

let answer = id (`Payload (0.0))
