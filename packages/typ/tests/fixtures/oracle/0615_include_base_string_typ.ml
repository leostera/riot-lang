(* oracle corpus fixture
   category: 14_schema_expansion
   title: include_base_string_typ
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, include
*)

module Base = struct
  type t = string
  let value : t = ("typ")
end

module Derived = struct
  include Base
  let pair x = (x, x)
end

let answer = Derived.pair Derived.value
