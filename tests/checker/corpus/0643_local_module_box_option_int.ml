(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_module_box_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_module
*)

type 'a option =
  | Some of 'a
  | None

let answer =
  let module M = struct
    type t = int option
    let value : t = (Some 0)
  end in
  M.value
