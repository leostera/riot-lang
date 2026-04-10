(* oracle corpus fixture
   category: 09_functors
   title: first_class_seed_bool
   complexity: 6
   min_ocaml: 4.08
   tags: modules, first_class_modules, package_type
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = bool
  let value : t = true
end

let packed = (module Seed : BOX with type t = bool)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of packed
