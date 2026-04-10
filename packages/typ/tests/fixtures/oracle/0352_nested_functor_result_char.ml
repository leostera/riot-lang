(* oracle corpus fixture
   category: 09_functors
   title: nested_functor_result_char
   complexity: 6
   min_ocaml: 4.08
   tags: modules, functors, nested_modules
*)

module type BOX = sig
  type t
  val value : t
end

module Lift (X : BOX) = struct
  module Inner = struct
    type t = X.t
    let value = X.value
  end
end

module Seed = struct
  type t = char
  let value : t = 'x'
end

module Result = Lift (Seed)

let answer = Result.Inner.value
