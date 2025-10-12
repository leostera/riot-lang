open Std
module Identifier = Typechecker.Identifier
module Location = Typechecker.Location

(** {1 Lambda Intermediate Representation}

    Lambda IR is the intermediate representation used after type checking. It's
    simpler than the typed AST but still high-level enough to optimize. *)

(** {2 Constants} *)

type structured_constant =
  | Const_int of int
  | Const_string of string
  | Const_float of float
  | Const_block of int * structured_constant list

(** {2 Primitive Operations} *)

type primitive =
  (* Integer arithmetic *)
  | Pint_add
  | Pint_sub
  | Pint_mul
  | Pint_div
  | Pint_mod
  | Pint_neg
  (* Integer comparisons *)
  | Pint_lt
  | Pint_le
  | Pint_gt
  | Pint_ge
  | Pint_eq
  | Pint_ne
  (* Memory operations *)
  | Pmakeblock of int
  | Pfield of int
  | Psetfield of int
  (* Boolean operations *)
  | Pnot
  (* Array operations *)
  | Pmakearray
  | Parraylength
  | Parrayrefu
  | Parraysetu

val primitive_to_string : primitive -> string
(** Convert primitive to readable name. *)

(** {2 Lambda Expressions} *)

type lambda =
  | Var of Identifier.t
  | Const of structured_constant
  | Apply of { func : lambda; args : lambda list; loc : Location.t option }
  | Function of {
      params : Identifier.t list;
      body : lambda;
      loc : Location.t option;
    }
  | Let of {
      id : Identifier.t;
      value : lambda;
      body : lambda;
      loc : Location.t option;
    }
  | LetRec of {
      bindings : (Identifier.t * lambda) list;
      body : lambda;
      loc : Location.t option;
    }
  | Prim of primitive * lambda list
  | IfThenElse of lambda * lambda * lambda option
  | Sequence of lambda * lambda
  | While of { condition : lambda; body : lambda; loc : Location.t option }
  | For of {
      id : Identifier.t;
      start : lambda;
      stop : lambda;
      direction : direction;
      body : lambda;
      loc : Location.t option;
    }
  | Switch of {
      scrutinee : lambda;
      cases : (int * lambda) list;
      default : lambda option;
      loc : Location.t option;
    }
  | StaticRaise of int * lambda list
  | StaticCatch of lambda * (int * Identifier.t list) * lambda

and direction = Upto | Downto

(** {2 Pretty Printing} *)

val lambda_to_string : lambda -> string
(** Convert Lambda IR to readable string (for debugging). *)

val const_to_string : structured_constant -> string
(** Convert constant to readable string. *)

(** {2 JSON Serialization} *)

val lambda_to_json : lambda -> Data.Json.t
(** Convert Lambda IR to JSON for output. *)

val const_to_json : structured_constant -> Data.Json.t
(** Convert constant to JSON. *)
