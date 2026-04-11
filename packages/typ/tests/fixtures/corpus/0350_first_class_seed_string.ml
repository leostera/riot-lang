(* oracle corpus fixture
   category: 09_functors
   title: first_class_seed_string
   complexity: 6
   min_ocaml: 4.08
   tags: modules, first_class_modules, package_type
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = string
  let value : t = "boxed"
end

let packed = (module Seed : BOX with type t = string)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of packed
