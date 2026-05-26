(* oracle corpus fixture
   category: 14_schema_expansion
   title: exception_raise_payload_pair_ib
   complexity: 6
   min_ocaml: 4.08
   tags: schema, exceptions, primitives
*)

module Prim = struct
  external raise_ : exn -> 'a = "%raise"
end

exception E of int * bool

let run flag =
  try
    if flag then
      Prim.raise_ (E ((0, true)))
    else
      ((0, false))
  with
  | E (left, right) -> (left, right)

let answer = run true
