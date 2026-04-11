(* oracle corpus fixture
   category: 05_variants
   title: qualified_constructor
   complexity: 3
   min_ocaml: 4.08
   tags: variants, modules, qualified_constructor
*)

module Packet = struct
  type t = Open of int | Closed
  let value = Open 0
end

let answer =
  match Packet.value with
  | Packet.Open x -> Packet.Open x
  | Packet.Closed -> Packet.Closed
