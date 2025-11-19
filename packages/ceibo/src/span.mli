(** Text spans and positions *)

open Std

type t = { start : int; end_ : int }
(** A span represents a range in source text *)

val make : start:int -> end_:int -> t
(** Create a span *)

val length : t -> int
(** Get the length of a span *)

val contains : t -> int -> bool
(** Check if a span contains an offset *)

val overlaps : t -> t -> bool
(** Check if two spans overlap *)

val union : t -> t -> t
(** Get the union of two spans *)

val to_string : t -> string
(** Convert to string *)

val to_json : t -> Data.Json.t
(** Convert to JSON *)
