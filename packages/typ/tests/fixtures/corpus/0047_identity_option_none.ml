(* oracle corpus fixture
   category: 01_basics
   title: identity_option_none
   complexity: 1
   min_ocaml: 4.08
   tags: basics, identity, 'aoption
*)

type 'a option =
  | Some of 'a
  | None

let value = None

let id x = x

let answer = id value
