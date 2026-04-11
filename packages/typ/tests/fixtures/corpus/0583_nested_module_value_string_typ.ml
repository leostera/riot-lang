(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_string_typ
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = string
    let value : t = ("typ")
  end
end

let answer = Outer.Inner.value
