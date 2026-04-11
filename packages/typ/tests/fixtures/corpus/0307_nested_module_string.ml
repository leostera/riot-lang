(* oracle corpus fixture
   category: 08_modules
   title: nested_module_string
   complexity: 3
   min_ocaml: 4.08
   tags: modules, nested_modules, paths
*)

module Outer = struct
  module Inner = struct
    type t = string
    let value : t = "typ"
  end
end

let answer = Outer.Inner.value
