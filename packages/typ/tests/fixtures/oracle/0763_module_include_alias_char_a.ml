(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_include_alias_char_a
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, include, alias
*)

module Base = struct
  type t = char
  let value : t = ('a')
end

module Alias = Base

module Derived = struct
  include Alias
  let id x = x
end

let answer = Derived.id Derived.value
