(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_char_z
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = char
    let value : t = ('z')
  end
end

let answer = Outer.Inner.value
