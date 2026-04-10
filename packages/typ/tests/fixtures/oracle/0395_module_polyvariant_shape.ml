(* oracle corpus fixture
   category: 11_polyvariants
   title: module_polyvariant_shape
   complexity: 6
   min_ocaml: 4.08
   tags: polyvariants, modules
*)

module M = struct
  type t = [ `Dot | `Line ]
  let flip = function
    | `Dot -> `Line
    | `Line -> `Dot
end

let answer = M.flip `Dot
