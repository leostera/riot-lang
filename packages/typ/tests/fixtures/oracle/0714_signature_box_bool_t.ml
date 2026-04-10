(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_bool_t
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = bool = struct
  type t = bool
  let value : t = (true)
end

let answer = M.value
