(* oracle corpus fixture
   category: 09_functors
   title: hybrid_string
   complexity: 7
   min_ocaml: 4.08
   tags: modules, functors, first_class_modules, hybrid
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
  type t = string
  let value : t = "hybrid"
end

module Built = Make (Seed)

let packed = (module Built : BOX with type t = string)

let answer =
  let module X = (val packed : BOX with type t = string) in
  X.value
