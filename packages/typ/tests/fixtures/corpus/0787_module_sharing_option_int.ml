(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_sharing_option_int
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

module M : BOX with type t = int option = struct
  type t = int option
  let value : t = (Some 0)
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
