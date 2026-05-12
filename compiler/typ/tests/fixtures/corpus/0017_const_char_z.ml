(* oracle corpus fixture
   category: 01_basics
   title: const_char_z
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, char
*)

let value = 'z'

let const x _ = x

let answer = const value ()
