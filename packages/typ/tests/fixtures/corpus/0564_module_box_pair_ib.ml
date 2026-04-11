(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_pair_ib
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

module M = struct
  type t = int * bool
  let value : t = ((0, true))
  let id (x : t) = x
end

let answer = M.id M.value
