(* oracle corpus fixture
   category: 13_primitives
   title: location_primitive_loc
   complexity: 4
   min_ocaml: 4.08
   tags: primitives, location
*)

module Prim = struct
  external value : string = "%loc_LOC"
end

let answer = Prim.value
