(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_unit
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = unit
    let value : t = (())
  end in
  M.value
