(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

type 'a option =
  | Some of 'a
  | None

let answer =
  let module M = struct
    type t = char option
    let value : t = (Some 'x')
  end in
  M.value
