(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_identity_answer_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row
*)

type t = [ `Yes | `No ]

let swap = function
  | `Yes -> `No
  | `No -> `Yes

let answer = swap `Yes
