(* oracle corpus fixture
   category: 08_modules
   title: signature_ascription_sig_string
   complexity: 4
   min_ocaml: 4.08
   tags: modules, signature, ascription
*)

module type S = sig
  type t
  val value : t
  val id : t -> t
end

module M : S with type t = string = struct
  type t = string
  let value = "x"
  let id x = x
end

let answer = M.id M.value
