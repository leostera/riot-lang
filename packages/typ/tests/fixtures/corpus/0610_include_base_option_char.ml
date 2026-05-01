(* oracle corpus fixture
   category: 14_schema_expansion
   title: include_base_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, include
*)

type 'a option =
  | Some of 'a
  | None

module Base = struct
  type t = char option
  let value : t = (Some 'x')
end

module Derived = struct
  include Base
  let pair x = (x, x)
end

let answer = Derived.pair Derived.value
