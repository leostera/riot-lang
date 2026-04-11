(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_identity_shape_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row
*)

type t = [ `Dot | `Line ]

let swap = function
  | `Dot -> `Line
  | `Line -> `Dot

let answer = swap `Dot
