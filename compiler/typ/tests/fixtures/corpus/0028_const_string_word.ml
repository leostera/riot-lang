(* oracle corpus fixture
   category: 01_basics
   title: const_string_word
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, string
*)

let value = "typ"

let const x _ = x

let answer = const value ()
