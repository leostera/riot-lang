(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = string * char = struct
  type t = string * char
  let value : t = (("x", 'y'))
end

let answer = M.value
