(* oracle corpus fixture
   category: 14_schema_expansion
   title: functor_pack_list_int
   complexity: 7
   min_ocaml: 4.08
   tags: schema, module, functor, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Make (X : BOX) = struct
  type t = X.t
  let value = X.value
end

module Seed = struct
  type t = int list
  let value : t = ([0; 1])
end

module Built = Make (Seed)

let packed = (module Built : BOX with type t = int list)

let answer =
  let module X = (val packed : BOX with type t = int list) in
  X.value
