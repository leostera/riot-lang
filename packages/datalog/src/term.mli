open Std

(** {1 Term - Datalog Terms}
    
    A term can be:
    - A variable (uppercase): X, Y, Person
    - A constant (concrete value): 42, "hello"
    - A wildcard (anonymous): _
*)

type t =
  | Var of string     (** Variable: X, Y, Foo *)
  | Const of Value.t  (** Constant: 42, "hello", uri:foo *)
  | Wildcard          (** Anonymous variable: _ *)

val compare : t -> t -> int
(** Total ordering for terms *)

val equal : t -> t -> bool
(** Equality check *)

val is_var : t -> bool
(** Check if term is a variable *)

val is_const : t -> bool
(** Check if term is a constant *)

val is_wildcard : t -> bool
(** Check if term is a wildcard *)

val var_name : t -> string option
(** Get variable name if term is a variable *)

val const_value : t -> Value.t option
(** Get constant value if term is a constant *)

val to_string : t -> string
(** Convert term to human-readable string *)

val vars : t -> string list
(** Get all variable names in this term (0 or 1 element) *)
