(* oracle corpus fixture
   category: 09_functors
   title: sharing_functor_pair
   complexity: 5
   min_ocaml: 4.08
   tags: modules, functors, with_type
*)

module type BOX = sig
  type t
  val value : t
end

module Make (X : BOX) : BOX with type t = X.t = struct
  type t = X.t
  let value = X.value
end

module Seed = struct
  type t = int * bool
  let value : t = (0, true)
end

module Result = Make (Seed)

let answer = Result.value
