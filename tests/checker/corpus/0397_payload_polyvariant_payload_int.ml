(* oracle corpus fixture
   category: 11_polyvariants
   title: payload_polyvariant_payload_int
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, payload
*)

let keep = function
  | `Count x -> `Count x
  | `Flag y -> `Flag y

let answer = keep (`Count 0)
