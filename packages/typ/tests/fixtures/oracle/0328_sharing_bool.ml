(* oracle corpus fixture
   category: 08_modules
   title: sharing_bool
   complexity: 5
   min_ocaml: 4.08
   tags: modules, type_sharing, manifest_type
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = bool = struct
  type t = bool
  let value = true
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
