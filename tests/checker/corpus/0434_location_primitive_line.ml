(* oracle corpus fixture
   category: 13_primitives
   title: location_primitive_line
   complexity: 4
   min_ocaml: 4.08
   tags: primitives, location
*)

module Prim = struct
  external value : int = "%loc_LINE"
end

let answer = Prim.value
