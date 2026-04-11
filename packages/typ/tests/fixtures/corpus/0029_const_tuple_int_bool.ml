(* oracle corpus fixture
   category: 01_basics
   title: const_tuple_int_bool
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, int*bool
*)

let value = (0, true)

let const x _ = x

let answer = const value ()
