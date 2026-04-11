(* oracle corpus fixture
   category: 08_modules
   title: signature_ascription_sig_pair
   complexity: 4
   min_ocaml: 4.08
   tags: modules, signature, ascription
*)

module type S = sig
  type t
  val value : t
  val id : t -> t
end

module M : S with type t = int * bool = struct
  type t = int * bool
  let value = (0, true)
  let id x = x
end

let answer = M.id M.value
