(* oracle corpus fixture
   category: 11_polyvariants
   title: row_combination_warm_cool
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, row_combination
*)

type left = [ `Red | `Yellow ]
type right = [ `Blue | `Green ]
type both = [ left | right ]

let id (value : both) : both = value

let answer = id `Red
