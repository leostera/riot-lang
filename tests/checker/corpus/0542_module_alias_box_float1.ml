(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_float1
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = float
  let value : t = (1.5)
end

module Alias = Source

let answer = Alias.value
