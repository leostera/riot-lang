(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_value_option_int
   complexity: 3
   min_ocaml: 4.08
   tags: schema, module, nested
*)

type 'a option =
  | Some of 'a
  | None

module Outer = struct
  module Inner = struct
    type t = int option
    let value : t = (Some 0)
  end
end

let answer = Outer.Inner.value
