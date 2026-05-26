(* oracle corpus fixture
   category: 01_basics
   title: const_nested_tuple
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, (int*int)*bool
*)

let value = ((0, 1), true)

let const x _ = x

let answer = const value ()
