(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_bool_t
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = bool
  let value : t = (true)
end

module Alias = Source

let answer = Alias.value
