(* oracle corpus fixture
   category: 01_basics
   title: const_int_one
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, int
*)

let value = 1

let const x _ = x

let answer = const value ()
