(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_include_alias_option_char
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, include, alias
*)

type 'a option =
  | Some of 'a
  | None

module Base = struct
  type t = char option
  let value : t = (Some 'x')
end

module Alias = Base

module Derived = struct
  include Alias
  let id x = x
end

let answer = Derived.id Derived.value
