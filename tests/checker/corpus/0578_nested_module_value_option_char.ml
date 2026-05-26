(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_option_char
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

type 'a option =
  | Some of 'a
  | None

module Outer = struct
  module Inner = struct
    type t = char option
    let value : t = (Some 'x')
  end
end

let answer = Outer.Inner.value
