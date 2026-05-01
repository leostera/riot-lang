(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_box_option_int
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, box
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = int option
  let value : t = (Some 0)
  let id (x : t) = x
end

let answer = M.id M.value
