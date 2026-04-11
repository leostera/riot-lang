(* oracle corpus fixture
   category: 13_primitives
   title: extensible_variant_message_string
   complexity: 6
   min_ocaml: 4.08
   tags: extensible_variants, exceptions
*)

type message = ..

type message += Payload of string
type message += Empty

let id = function
  | Payload x -> Payload x
  | Empty -> Empty

let answer = id (Payload "msg")
