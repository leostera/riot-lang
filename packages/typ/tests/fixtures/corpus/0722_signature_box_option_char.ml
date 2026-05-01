(* oracle corpus fixture
   category: 14_schema_expansion
   title: signature_box_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, signature
*)

type 'a option =
  | Some of 'a
  | None

module type BOX = sig
  type t
  val value : t
end

module M : BOX with type t = char option = struct
  type t = char option
  let value : t = (Some 'x')
end

let answer = M.value
