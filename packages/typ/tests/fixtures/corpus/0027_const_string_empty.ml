(* oracle corpus fixture
   category: 01_basics
   title: const_string_empty
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, string
*)

let value = ""

let const x _ = x

let answer = const value ()
