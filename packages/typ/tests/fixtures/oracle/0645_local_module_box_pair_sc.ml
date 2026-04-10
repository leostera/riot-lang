(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = string * char
    let value : t = (("x", 'y'))
  end in
  M.value
