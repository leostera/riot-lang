open Std

(** {1 Query - Pattern Matching and Searches}
    
    Query types and helpers for pattern-based searches.
*)

type pattern =
  | Exact of Uri.t  (** Match exact URI *)
  | Any  (** Match anything *)
  | Variable of string  (** Bind to variable *)

type query_result = {
  entity : Uri.t;
  attribute : Uri.t;
  value : Fact.value;
  bindings : (string * Fact.value) list;
}

val value_equal : Fact.value -> Fact.value -> bool
(** Check if two values are equal *)
