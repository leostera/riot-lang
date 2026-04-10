(* oracle corpus fixture
   category: 01_basics
   title: identity_string_empty
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, string
*)

let value = ""

let id x = x

let answer = id value
