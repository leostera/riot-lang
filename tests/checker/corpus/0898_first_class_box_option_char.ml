(* oracle corpus fixture
   category: 14_schema_expansion
   title: first_class_box_option_char
   complexity: 6
   min_ocaml: 4.08
   tags: schema, module, first_class_module
*)

type 'a option =
  | Some of 'a
  | None

module type BOX = sig
  type t
  val value : t
end

module Seed = struct
  type t = char option
  let value : t = (Some 'x')
end

let packed = (module Seed : BOX with type t = char option)

let answer =
  let module X = (val packed : BOX with type t = char option) in
  X.value
