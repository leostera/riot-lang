(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_include_alias_pair_sc
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, include, alias
*)

module Base = struct
  type t = string * char
  let value : t = (("x", 'y'))
end

module Alias = Base

module Derived = struct
  include Alias
  let id x = x
end

let answer = Derived.id Derived.value
