(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_box_bool_t
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = bool
  let value : t = (true)
end

let packed = (module Seed : BOX with type t = bool)

let answer =
  let module X = (val packed : BOX with type t = bool) in
  X.value
