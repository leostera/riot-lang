(* oracle corpus fixture
   category: 11_polyvariants
   title: module_polyvariant_color
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, modules
*)

module M = struct
  type t = [ `Red | `Blue ]
  let flip = function
    | `Red -> `Blue
    | `Blue -> `Red
end

let answer = M.flip `Red
