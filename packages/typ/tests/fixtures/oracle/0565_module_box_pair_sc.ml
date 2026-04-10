(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_pair_sc
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

module M = struct
  type t = string * char
  let value : t = (("x", 'y'))
  let id (x : t) = x
end

let answer = M.id M.value
