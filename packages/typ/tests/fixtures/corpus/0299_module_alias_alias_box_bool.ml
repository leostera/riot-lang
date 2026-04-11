(* oracle corpus fixture
   category: 08_modules
   title: module_alias_alias_box_bool
   complexity: 3
   min_ocaml: 4.08
   tags: modules, alias, paths
*)

module Source = struct
  type t = bool
  let value : t = true
end

module Alias = Source

let answer = Alias.value
