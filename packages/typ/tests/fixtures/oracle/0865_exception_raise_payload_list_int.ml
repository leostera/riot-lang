(* oracle corpus fixture
   category: 14_schema_expansion
   title: exception_raise_payload_list_int
   complexity: 6
   min_ocaml: 4.08
   tags: schema, exceptions, primitives
*)

module Prim = struct
  external raise_ : exn -> 'a = "%raise"
end

exception E of int list

let run flag =
  try
    if flag then
      Prim.raise_ (E ([0; 1]))
    else
      ([])
  with
  | E value -> value

let answer = run true
