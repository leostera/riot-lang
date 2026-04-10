(* oracle corpus fixture
   category: 01_basics
   title: const_option_some
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, intoption
*)

let value = Some 0

let const x _ = x

let answer = const value ()
