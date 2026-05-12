(* oracle corpus fixture
   category: 01_basics
   title: const_list_empty
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, 'alist
*)

let value = []

let const x _ = x

let answer = const value ()
