(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_char_z
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = char = struct
  type t = char
  let value : t = ('z')
end

let answer = M.value
