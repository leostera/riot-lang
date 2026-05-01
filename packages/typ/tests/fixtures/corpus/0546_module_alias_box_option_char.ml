(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_option_char
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

type 'a option =
  | Some of 'a
  | None

module Source = struct
  type t = char option
  let value : t = (Some 'x')
end

module Alias = Source

let answer = Alias.value
