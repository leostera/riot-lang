(* oracle corpus fixture
   category: 14_schema_expansion
   title: include_base_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, include
*)

module Base = struct
  type t = string * char
  let value : t = (("x", 'y'))
end

module Derived = struct
  include Base
  let pair x = (x, x)
end

let answer = Derived.pair Derived.value
