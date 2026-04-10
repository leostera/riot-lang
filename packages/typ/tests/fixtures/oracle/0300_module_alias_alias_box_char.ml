(* oracle corpus fixture
   category: 08_modules
   title: module_alias_alias_box_char
   complexity: 3
   min_ocaml: 4.08
   tags: modules, alias, paths
*)

module Source = struct
  type t = char
  let value : t = 'a'
end

module Alias = Source

let answer = Alias.value
