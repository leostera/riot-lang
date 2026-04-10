(* oracle corpus fixture
   category: 01_basics
   title: identity_list_empty
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, 'alist
*)

let value = []

let id x = x

let answer = id value
