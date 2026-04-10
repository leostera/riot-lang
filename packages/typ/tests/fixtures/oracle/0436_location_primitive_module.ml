(* oracle corpus fixture
   category: 13_primitives
   title: location_primitive_module
   complexity: 4
   min_ocaml: 4.08
   tags: primitives, location
*)

module Prim = struct
  external value : string = "%loc_MODULE"
end

let answer = Prim.value
