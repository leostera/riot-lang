(* oracle corpus fixture
   category: 11_polyvariants
   title: polyvariant_widen_basic_extended_answer
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, coercion, row_subtyping
*)

type small = [ `Yes | `No ]
type large = [ `Yes | `No | `Maybe ]

let widen (value : small) : large = (value :> large)

let answer = widen `Yes
