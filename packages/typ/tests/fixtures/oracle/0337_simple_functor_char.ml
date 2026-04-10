(* oracle corpus fixture
   category: 09_functors
   title: simple_functor_char
   complexity: 5
   min_ocaml: 4.08
   tags: modules, functors, signature
*)

module type BOX = sig
  type t
  val value : t
end

module Make (X : BOX) = struct
  type t = X.t
  let value = X.value
  let id (x : t) = x
end

module Seed = struct
  type t = char
  let value : t = 'x'
end

module Result = Make (Seed)

let answer = Result.id Result.value
