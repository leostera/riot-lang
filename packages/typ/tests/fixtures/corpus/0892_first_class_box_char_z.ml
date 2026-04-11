(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_box_char_z
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = char
  let value : t = ('z')
end

let packed = (module Seed : BOX with type t = char)

let answer =
  let module X = (val packed : BOX with type t = char) in
  X.value
