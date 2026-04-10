(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_alias_box_pair_sc
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, alias
*)

module Source = struct
  type t = string * char
  let value : t = (("x", 'y'))
end

module Alias = Source

let answer = Alias.value
