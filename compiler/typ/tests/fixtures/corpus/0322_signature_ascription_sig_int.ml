(* oracle corpus fixture
   category: 08_modules
   title: signature_ascription_sig_int
   complexity: 4
   min_ocaml: 4.08
   tags: modules, signature, ascription
*)

module type S = sig
  type t
  val value : t
  val id : t -> t
end

module M : S with type t = int = struct
  type t = int
  let value = 0
  let id x = x
end

let answer = M.id M.value
