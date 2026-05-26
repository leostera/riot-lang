(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_bool_t
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = bool
    let value : t = (true)
  end
end

let answer = Outer.Inner.value
