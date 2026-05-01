(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_sharing_option_char
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, sharing
*)

type 'a option =
  | Some of 'a
  | None

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = char option = struct
  type t = char option
  let value : t = (Some 'x')
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
