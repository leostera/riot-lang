(* oracle corpus fixture
   category: 14_schema_expansion
   title: include_base_bool_t
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, include
*)

module Base = struct
  type t = bool
  let value : t = (true)
end

module Derived = struct
  include Base
  let pair x = (x, x)
end

let answer = Derived.pair Derived.value
