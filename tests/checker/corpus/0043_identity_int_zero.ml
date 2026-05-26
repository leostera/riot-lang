(* oracle corpus fixture
   category: 01_basics
   title: identity_int_zero
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, int
*)

let value = 0

let id x = x

let answer = id value
