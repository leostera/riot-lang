(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_int0
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = int = struct
  type t = int
  let value : t = (0)
end

let answer = M.value
