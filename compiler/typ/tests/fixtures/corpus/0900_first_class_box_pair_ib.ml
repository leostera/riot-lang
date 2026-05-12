(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_box_pair_ib
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = int * bool
  let value : t = ((0, true))
end

let packed = (module Seed : BOX with type t = int * bool)

let answer =
  let module X = (val packed : BOX with type t = int * bool) in
  X.value
