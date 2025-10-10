(* Comprehensive trivia preservation test *)
(* This file combines multiple constructs to ensure trivia is never lost *)

(** Docstring for module *)
module Test = struct
  (** Docstring for type *)
  type 'a (* type param *) t (* name *) = 
    | None (* no value *)
    | Some (* has value *) of (* of keyword *) 'a (* payload *)

  (** Record type with all the trivia *)
  type record = {
    (* Field x *)
    mutable (* mutable *) x (* name *) : (* colon *) int (* type *) ; (* semi *)
    (* Field y *)
    y : string;
  }

  (** Function with complex signature *)
  let f 
    (* First parameter *)
    ~label1 (* labeled *) : (* colon *) int (* type *)
    (* Second parameter *)
    ?label2 (* optional *) : (* colon *) (string (* inner *)) (* type *)
    (* Third parameter *)
    x (* regular *)
    (* Return type *)
    : (* colon *) bool (* result type *)
    =
    (* Function body *)
    x > 0 (* condition *)

  (** Nested pattern matching *)
  let rec eval = function
    | None (* pattern *) -> (* arrow *) 0 (* result *)
    | Some (* constructor *) (
        (* nested pattern *)
        (a (* first *), (* comma *) b (* second *))
      ) (* close paren *) 
      when (* when *) a > 0 (* guard *)
      -> (* arrow *) a + b (* body *)

  (** Module with functor *)
  module type S = sig
    type t
    val x : t
  end

  module F (* name *)
    (X (* param *) : (* colon *) S (* signature *))
    : (* constraint *) S (* output sig *)
  = struct
    type t = X.t
    let x = X.x
  end
end
