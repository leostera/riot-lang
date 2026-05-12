(* oracle corpus fixture
   category: 01_basics
   title: const_char_a
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, char
*)

let value = 'a'

let const x _ = x

let answer = const value ()
