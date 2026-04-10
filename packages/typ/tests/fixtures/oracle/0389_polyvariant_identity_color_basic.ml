(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_identity_color_basic
   complexity: 5
   min_ocaml: 4.08
   tags: polyvariants, closed_row
*)

type t = [ `Red | `Blue ]

let swap = function
  | `Red -> `Blue
  | `Blue -> `Red

let answer = swap `Red
