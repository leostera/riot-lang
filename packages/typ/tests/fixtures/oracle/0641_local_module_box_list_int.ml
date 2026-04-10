(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_list_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = int list
    let value : t = ([0; 1])
  end in
  M.value
