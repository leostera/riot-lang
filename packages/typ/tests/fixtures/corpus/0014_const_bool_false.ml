(* oracle corpus fixture
   category: 01_basics
   title: const_bool_false
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, bool
*)

let value = false

let const x _ = x

let answer = const value ()
