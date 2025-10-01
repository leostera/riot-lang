(** Global functions *)

val panic : string -> 'a
(** Raise a panic exception with the given message *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)
