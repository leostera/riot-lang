(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_widen_basic_extended_shape
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, coercion, row_subtyping
*)

type small = [ `Dot | `Line ]
type large = [ `Dot | `Line | `Area ]

let widen (value : small) : large = (value :> large)

let answer = widen `Line
