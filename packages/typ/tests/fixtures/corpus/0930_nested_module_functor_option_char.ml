(* oracle corpus fixture
   category: 14_schema_expansion
   title: nested_module_functor_option_char
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, functor, nested
*)

type 'a option =
  | Some of 'a
  | None

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
  type t = char option
  let value : t = (Some 'x')
end

module Result = Lift (Seed)

let answer = Result.Inner.value
