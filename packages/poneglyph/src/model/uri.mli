open Std

type t
(** Opaque URI - automatically interned for fast equality *)

type part =
  | Ns of string
  | Kind of string
  | Id of string
  | Field of string  (** URI components *)

val of_string : string -> t
(** Create URI from string. Handles @ shorthand (@ = poneglyph:).
    Same string always returns same URI (interned). *)

val make : part list -> t
(** Create URI from parts, e.g. [Ns "tusk"; Kind "file"; Id "foo.ml"] becomes
    "tusk:kind:file:foo.ml" *)

val to_string : t -> string
(** Convert URI back to string *)

val equal : t -> t -> bool
(** Fast URI equality (integer comparison) *)

val compare : t -> t -> int
(** Fast URI comparison (integer comparison) *)

val ns : string -> part
(** Namespace part *)

val kind : string -> part
(** Kind part *)

val id : ('a, unit, string, part) format4 -> 'a
(** ID part with format support *)

val field : string -> part
(** Field name part *)
