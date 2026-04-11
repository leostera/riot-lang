(* oracle corpus fixture
   category: 08_modules
   title: sharing_string
   complexity: 5
   min_ocaml: 4.08
   tags: modules, type_sharing, manifest_type
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = string = struct
  type t = string
  let value = "share"
end

module Use = struct
  let project (x : M.t) = x
end

let answer = Use.project M.value
