(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_list_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = int list = struct
  type t = int list
  let value : t = ([0; 1])
end

let answer = M.value
