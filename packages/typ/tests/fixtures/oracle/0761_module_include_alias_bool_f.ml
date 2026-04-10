(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_include_alias_bool_f
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, include, alias
*)

module Base = struct
  type t = bool
  let value : t = (false)
end

module Alias = Base

module Derived = struct
  include Alias
  let id x = x
end

let answer = Derived.id Derived.value
