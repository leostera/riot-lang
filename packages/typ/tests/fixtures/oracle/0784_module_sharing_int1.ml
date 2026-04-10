(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_sharing_int1
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, sharing
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = int = struct
  type t = int
  let value : t = (1)
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
