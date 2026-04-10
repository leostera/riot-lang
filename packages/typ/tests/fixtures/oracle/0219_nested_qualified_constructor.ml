(* oracle corpus fixture
   category: 05_variants
   title: nested_qualified_constructor
   complexity: 3
   min_ocaml: 4.08
   tags: variants, modules, qualified_constructor
*)

module Outer = struct
  module Inner = struct
    type t = Wrap of bool
    let value = Wrap true
  end
end

let answer =
  match Outer.Inner.value with
  | Outer.Inner.Wrap flag -> Outer.Inner.Wrap flag
