(* oracle corpus fixture
   category: 01_basics
   title: identity_float_zero
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, float
*)

let value = 0.0

let id x = x

let answer = id value
