(** Text spans and positions *)
open Std

(** A span represents a range in source text *)
(** Create a span *)
type t = {
  start: int;
  end_: int;
}
val make: start:int -> end_:int -> t
(** Get the length of a span *)
val length: t -> int
(** Check if a span contains an offset *)
val contains: t -> int -> bool
(** Check if two spans overlap *)
val overlaps: t -> t -> bool
(** Get the union of two spans *)
val union: t -> t -> t
(** Convert to string *)
val to_string: t -> string
(** Convert to JSON *)
val to_json: t -> Data.Json.t
