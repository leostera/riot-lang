(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_pair_ib
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = int * bool
  let value : t = ((0, true))
end

module Alias = Source

let answer = Alias.value
