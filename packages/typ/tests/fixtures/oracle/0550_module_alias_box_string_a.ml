(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_string_a
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = string
  let value : t = ("a")
end

module Alias = Source

let answer = Alias.value
