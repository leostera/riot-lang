(* oracle corpus fixture
   category: 01_basics
   title: const_option_none
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, 'aoption
*)

let value = None

let const x _ = x

let answer = const value ()
