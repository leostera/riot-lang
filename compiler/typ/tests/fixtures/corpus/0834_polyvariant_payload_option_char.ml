(* oracle corpus fixture
   category: 14_schema_expansion
   title: polyvariant_payload_option_char
   complexity: 5
   min_ocaml: 4.08
   tags: schema, polyvariant, payload
*)

type 'a option =
  | Some of 'a
  | None

let id = function
  | `Payload x -> `Payload x

let answer = id (`Payload (Some 'x'))
