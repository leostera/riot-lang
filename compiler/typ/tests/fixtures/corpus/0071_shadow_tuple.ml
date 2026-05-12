(* oracle corpus fixture
   category: 01_basics
   title: shadow_tuple
   complexity: 2
   min_ocaml: 4.08
   tags: basics, let, shadowing
*)

let seed = (0, false)

let answer =
  let value = seed in
  let value = (value, seed) in
  value
