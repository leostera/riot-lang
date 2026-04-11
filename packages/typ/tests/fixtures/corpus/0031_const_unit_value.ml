(* oracle corpus fixture
   category: 01_basics
   title: const_unit_value
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, unit
*)

let value = ()

let const x _ = x

let answer = const value ()
