(* oracle corpus fixture
   category: 01_basics
   title: shadow_int
   complexity: 2
   min_ocaml: 4.08
   tags: basics, let, shadowing
*)

let seed = 0

let answer =
  let value = seed in
  let value = (value, seed) in
  value
