(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_box_float1
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = float
  let value : t = (1.5)
end

let packed = (module Seed : BOX with type t = float)

let answer =
  let module X = (val packed : BOX with type t = float) in
  X.value
