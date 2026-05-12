(* oracle corpus fixture
   category: 01_basics
   title: identity_unit_value
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, unit
*)

let value = ()

let id x = x

let answer = id value
