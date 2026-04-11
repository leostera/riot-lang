(* oracle corpus fixture
   category: 11_polyvariants
   title: payload_polyvariant_payload_pair
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, payload
*)

let keep = function
  | `Pair x -> `Pair x
  | `Unit y -> `Unit y

let answer = keep (`Pair (0, true))
