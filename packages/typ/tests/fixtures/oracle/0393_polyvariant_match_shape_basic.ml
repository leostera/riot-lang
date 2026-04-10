(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_match_shape_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row, match
*)

type t = [ `Dot | `Line ]

let id = function
  | `Dot -> `Dot
  | `Line -> `Line

let answer = id `Line
