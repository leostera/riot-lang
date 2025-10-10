(* Test that trivia is preserved in module types *)

(* Module type with comment *)
module type S = sig
  (* This is a value *)
  val x : int
end

(* Functor type with trivia *)
module type F = functor
  (* input module *)
  (X : S) 
  (* arrow *)
  -> 
  (* output *)
  S

(* With constraint and comments *)
module type T = S with type t = int (* constraint *)

(* Multiple constraints with trivia *)
module type U = S 
  with type t = int (* first *)
  and type u = string (* second *)
