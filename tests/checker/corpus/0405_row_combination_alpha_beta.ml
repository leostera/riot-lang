(* oracle corpus fixture
   category: 11_polyvariants
   title: row_combination_alpha_beta
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_combination
*)

type left = [ `Alpha | `Beta ]
type right = [ `Gamma | `Delta ]
type both = [ left | right ]

let id (value : both) : both = value

let answer = id `Alpha
