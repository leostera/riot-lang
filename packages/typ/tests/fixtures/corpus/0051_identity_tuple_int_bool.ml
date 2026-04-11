(* oracle corpus fixture
   category: 01_basics
   title: identity_tuple_int_bool
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, int*bool
*)

let value = (0, true)

let id x = x

let answer = id value
