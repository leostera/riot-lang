(* oracle corpus fixture
   category: 11_polyvariants
   title: payload_polyvariant_payload_char
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, payload
*)

let keep = function
  | `Mark x -> `Mark x
  | `Word y -> `Word y

let answer = keep (`Mark 'a')
