(* oracle corpus fixture
   category: 01_basics
   title: identity_tuple_string_char
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, string*char
*)

let value = ("x", 'y')

let id x = x

let answer = id value
