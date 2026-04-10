(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_bool_f
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = bool
    let value : t = (false)
  end in
  M.value
