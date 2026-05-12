(* oracle corpus fixture
   category: 01_basics
   title: identity_list_ints
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, intlist
*)

let value = [0; 1; 2]

let id x = x

let answer = id value
