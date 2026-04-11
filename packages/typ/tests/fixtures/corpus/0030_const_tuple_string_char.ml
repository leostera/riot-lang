(* oracle corpus fixture
   category: 01_basics
   title: const_tuple_string_char
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, string*char
*)

let value = ("x", 'y')

let const x _ = x

let answer = const value ()
