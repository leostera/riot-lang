(* oracle corpus fixture
   category: 09_functors
   title: first_class_module_int
   complexity: 6
   min_ocaml: 4.08
   tags: modules, first_class_modules, package_type
*)

module type BOX = sig
  type t
  val value : t
  val id : t -> t
end

module Seed = struct
  type t = int
  let value : t = 0
  let id x = x
end

let packed = (module Seed : BOX with type t = int)

let run (type a) (module X : BOX with type t = a) (x : a) =
  X.id x

let answer = run packed Seed.value
