(* oracle corpus fixture
   category: 01_basics
   title: const_int_zero
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, int
*)

let value = 0

let const x _ = x

let answer = const value ()
