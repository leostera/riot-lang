(* oracle corpus fixture
   category: 01_basics
   title: identity_nested_tuple
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, (int*int)*bool
*)

let value = ((0, 1), true)

let id x = x

let answer = id value
