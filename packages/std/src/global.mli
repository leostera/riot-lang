(** Global functions *)

val panic : string -> 'a
(** Raise a panic exception with the given message *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)

val format : ('a, unit, string, string) format4 -> 'a
(** Format string helper - alias for format *)

val print : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with immediate flush *)

val println : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with newline and immediate flush *)
