(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_sharing_string_typ
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, sharing
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = string = struct
  type t = string
  let value : t = ("typ")
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
