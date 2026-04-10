(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_int1
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = int
  let value : t = (1)
end

module Alias = Source

let answer = Alias.value
