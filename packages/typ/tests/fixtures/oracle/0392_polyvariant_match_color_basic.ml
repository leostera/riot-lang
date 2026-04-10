(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_match_color_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row, match
*)

type t = [ `Red | `Blue ]

let id = function
  | `Red -> `Red
  | `Blue -> `Blue

let answer = id `Blue
