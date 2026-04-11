(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_list_int
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

module Outer = struct
  module Inner = struct
    type t = int list
    let value : t = ([0; 1])
  end
end

let answer = Outer.Inner.value
