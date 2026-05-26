(* oracle corpus fixture
   category: 01_basics
   title: const_option_none
   complexity: 1
   min_ocaml: 4.08
   tags: basics, const, 'aoption
*)

type 'a option =
  | Some of 'a
  | None

let value = None

let const x _ = x

let answer = const value ()
