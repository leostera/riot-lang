(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_bool_t
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

module M = struct
  type t = bool
  let value : t = (true)
  let id (x : t) = x
end

let answer = M.id M.value
