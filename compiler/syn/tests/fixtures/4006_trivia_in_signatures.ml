(* Test that trivia is preserved in signature items *)

(* Module signature with trivia *)

module type S = sig
  (* Value declarations with comments *)

  val x: int

  (* Type declarations *)

  type t
  (* abstract type *)

  (* Type with definition *)

  type u = int
  (* concrete type *)

  (* External declarations *)

  external add: int -> int -> int = "caml_add"

  (* Module declarations *)

  module M: sig
    val y: string
  end

  (* Include *)

  include sig
    val z: bool
  end

  (* Open *)

  open List
end
