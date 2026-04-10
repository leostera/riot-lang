(* oracle corpus fixture
   category: 08_modules
   title: nested_module_char
   complexity: 3
   min_ocaml: 4.08
   tags: modules, nested_modules, paths
*)

module Outer = struct
  module Inner = struct
    type t = char
    let value : t = 'x'
  end
end

let answer = Outer.Inner.value
