(* oracle corpus fixture
   category: 01_basics
   title: const_bool_true
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, bool
*)

let value = true

let const x _ = x

let answer = const value ()
