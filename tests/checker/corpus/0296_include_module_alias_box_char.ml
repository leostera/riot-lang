(* oracle corpus fixture
   category: 08_modules
   title: include_module_alias_box_char
   complexity: 3
   min_ocaml: 4.08
   tags: modules, include, structures
*)

module Base = struct
  type t = char
  let value : t = 'a'
end

module Derived = struct
  include Base
  let duplicate x = (x, x)
end

let answer = Derived.duplicate Derived.value
