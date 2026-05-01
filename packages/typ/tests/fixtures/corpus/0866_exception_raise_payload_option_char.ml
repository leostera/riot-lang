(* oracle corpus fixture
   category: 14_schema_expansion
   title: exception_raise_payload_option_char
   complexity: 6
   min_ocaml: 4.08
   tags: schema, exceptions, primitives
*)

type 'a option =
  | Some of 'a
  | None

module Prim = struct
  external raise_ : exn -> 'a = "%raise"
end

exception E of char option

let run flag =
  try
    if flag then
      Prim.raise_ (E (Some 'x'))
    else
      (None)
  with
  | E value -> value

let answer = run true
