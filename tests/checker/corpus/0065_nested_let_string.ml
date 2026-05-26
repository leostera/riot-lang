(* oracle corpus fixture
   category: 01_basics
   title: nested_let_string
   complexity: 2
   min_ocaml: 4.08
   tags: basics, let, nesting
*)

let seed = "shadow"

let answer =
  let first = seed in
  let second = first in
  second
