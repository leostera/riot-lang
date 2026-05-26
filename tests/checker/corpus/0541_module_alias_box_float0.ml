(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_float0
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = float
  let value : t = (0.0)
end

module Alias = Source

let answer = Alias.value
