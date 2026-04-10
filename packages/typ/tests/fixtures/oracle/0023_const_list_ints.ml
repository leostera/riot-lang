(* oracle corpus fixture
   category: 01_basics
   title: const_list_ints
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, intlist
*)

let value = [0; 1; 2]

let const x _ = x

let answer = const value ()
