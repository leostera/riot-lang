(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_float1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = float
    let value : t = (1.5)
  end in
  M.value
