(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_int1
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
  let value : t = (1)
end

let answer = M.value
