(* oracle corpus fixture
   category: 01_basics
   title: const_float_zero
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, float
*)

let value = 0.0

let const x _ = x

let answer = const value ()
