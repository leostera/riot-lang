(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_unit
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = unit = struct
  type t = unit
  let value : t = (())
end

let answer = M.value
