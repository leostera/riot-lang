(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_int1
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = int
    let value : t = (1)
  end
end

let answer = Outer.Inner.value
