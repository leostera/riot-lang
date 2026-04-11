(* oracle corpus fixture
   category: 09_functors
   title: sharing_functor_char
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
  type t = char
  let value : t = 'x'
end

module Result = Make (Seed)

let answer = Result.value
