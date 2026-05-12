(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_match_answer_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row, match
*)

type t = [ `Yes | `No ]

let id = function
  | `Yes -> `Yes
  | `No -> `No

let answer = id `No
