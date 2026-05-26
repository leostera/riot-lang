(* oracle corpus fixture
   category: 01_basics
   title: shadow_char
   complexity: 2
   min_ocaml: 4.08
   tags: basics, let, shadowing
*)

let seed = 'x'

let answer =
  let value = seed in
  let value = (value, seed) in
  value
