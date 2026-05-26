(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_option_int
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

type 'a option =
  | Some of 'a
  | None

module Source = struct
  type t = int option
  let value : t = (Some 0)
end

module Alias = Source

let answer = Alias.value
