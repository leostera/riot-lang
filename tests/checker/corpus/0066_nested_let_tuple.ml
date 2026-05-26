(* oracle corpus fixture
   category: 01_basics
   title: nested_let_tuple
   complexity: 2
   min_ocaml: 4.08
   tags: basics, let, nesting
*)

let seed = (0, false)

let answer =
  let first = seed in
  let second = first in
  second
