(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_string_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = string
    let value : t = ("a")
  end in
  M.value
