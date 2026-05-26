(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_unit
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = unit
  let value : t = (())
end

module Alias = Source

let answer = Alias.value
