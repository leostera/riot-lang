(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_float1
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

module M = struct
  type t = float
  let value : t = (1.5)
  let id (x : t) = x
end

let answer = M.id M.value
