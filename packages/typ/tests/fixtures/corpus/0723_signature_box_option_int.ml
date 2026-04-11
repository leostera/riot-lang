(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = int option = struct
  type t = int option
  let value : t = (Some 0)
end

let answer = M.value
