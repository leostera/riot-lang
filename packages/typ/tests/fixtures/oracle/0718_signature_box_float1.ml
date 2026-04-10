(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_float1
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = float = struct
  type t = float
  let value : t = (1.5)
end

let answer = M.value
