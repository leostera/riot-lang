(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_option_char
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = char option
  let value : t = (Some 'x')
  let id (x : t) = x
end

let answer = M.id M.value
