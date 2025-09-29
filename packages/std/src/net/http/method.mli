(** HTTP request methods **)

type t =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch
  | Extension of string

val of_string : string -> t
(** Parse an HTTP method from string **)

val to_string : t -> string
(** Convert HTTP method to string **)

val is_safe : t -> bool
(** Check if the method is safe (GET, HEAD, OPTIONS, TRACE) **)

val is_idempotent : t -> bool
(** Check if the method is idempotent **)

val is_cacheable : t -> bool
(** Check if responses to this method can be cached **)

val compare : t -> t -> int
(** Compare two HTTP methods **)

val equal : t -> t -> bool
(** Check if two HTTP methods are equal **)
