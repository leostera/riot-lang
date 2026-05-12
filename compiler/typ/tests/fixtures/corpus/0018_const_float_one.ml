(* oracle corpus fixture
   category: 01_basics
   title: const_float_one
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, float
*)

let value = 1.5

let const x _ = x

let answer = const value ()
