(* oracle corpus fixture
   category: 14_schema_expansion
   title: functor_box_option_int
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, functor
*)

type 'a option =
  | Some of 'a
  | None

module type BOX = sig
  type t
  val value : t
end

module Make (X : BOX) = struct
  type t = X.t
  let value = X.value
end

module Seed = struct
  type t = int option
  let value : t = (Some 0)
end

module Result = Make (Seed)

let answer = Result.value
