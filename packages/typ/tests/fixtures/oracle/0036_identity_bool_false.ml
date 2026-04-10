(* oracle corpus fixture
   category: 01_basics
   title: identity_bool_false
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, bool
*)

let value = false

let id x = x

let answer = id value
