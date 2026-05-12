(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_string_a
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

module M = struct
  type t = string
  let value : t = ("a")
  let id (x : t) = x
end

let answer = M.id M.value
