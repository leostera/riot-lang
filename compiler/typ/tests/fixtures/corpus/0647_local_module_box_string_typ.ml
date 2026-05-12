(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_string_typ
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = string
    let value : t = ("typ")
  end in
  M.value
