(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_char_z
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

let answer =
  let module M = struct
    type t = char
    let value : t = ('z')
  end in
  M.value
