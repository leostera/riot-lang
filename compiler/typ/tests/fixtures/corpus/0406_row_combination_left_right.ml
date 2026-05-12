(* oracle corpus fixture
   category: 11_polyvariants
   title: row_combination_left_right
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_combination
*)

type left = [ `Left | `Center ]
type right = [ `Right | `Far ]
type both = [ left | right ]

let id (value : both) : both = value

let answer = id `Left
