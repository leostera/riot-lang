(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_seed_list_int
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = int list
  let value : t = ([0; 1])
end

let packed = (module Seed : BOX with type t = int list)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of packed
