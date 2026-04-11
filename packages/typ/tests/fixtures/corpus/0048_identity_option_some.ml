(* oracle corpus fixture
   category: 01_basics
   title: identity_option_some
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, intoption
*)

let value = Some 0

let id x = x

let answer = id value
