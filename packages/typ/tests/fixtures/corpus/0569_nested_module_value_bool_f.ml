(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_bool_f
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = bool
    let value : t = (false)
  end
end

let answer = Outer.Inner.value
