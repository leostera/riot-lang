open Std

(** A half-open source range. *)
type t = { start: int; end_: int }

(** Create a span from two offsets. *)
val make: start:int -> end_:int -> t

(** Return the width of the span in source offsets. *)
val length: t -> int

(** Return `true` if the span contains the offset. *)
val contains: t -> int -> bool

(** Return `true` if the spans overlap. *)
val overlaps: t -> t -> bool

(** Return the smallest span covering both inputs. *)
val union: t -> t -> t

(** Move a span by an offset delta. *)
val shift: t -> by:int -> t

(** Format a span for diagnostics. *)
val to_string: t -> string

(** Encode a span as JSON. *)
val to_json: t -> Data.Json.t
