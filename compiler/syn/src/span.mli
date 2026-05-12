(** Source-text spans and offset-based position helpers. *)
open Std

(** A half-open range in source text. *)
type t = { start: int; end_: int }

(** Create a span from two offsets. *)
val make: start:int -> end_:int -> t

(** Return the width of the span in offsets. *)
val width: t -> int

(** Alias for `width`. *)
val length: t -> int

(**
   Compare spans by width.

   Shorter spans sort before longer spans. Spans with the same width compare
   equal regardless of where they start.
*)
val compare: t -> t -> Order.t

(**
   Return `true` if the first span fully contains the second span.

   Spans are half-open ranges. A zero-width span is treated as a cursor
   position and may sit on the containing span's end boundary.
*)
val contains: t -> t -> bool

(** Return `true` if the span contains the given offset. *)
val contains_offset: t -> int -> bool

(** Return `true` if the two spans overlap. *)
val overlaps: t -> t -> bool

(** Return `true` if the first span starts before the second span. *)
val starts_before: t -> t -> bool

(** Return `true` if the first span ends before the second span. *)
val ends_before: t -> t -> bool

(** Return `true` if the first span starts after the second span. *)
val starts_after: t -> t -> bool

(** Return `true` if the first span ends after the second span. *)
val ends_after: t -> t -> bool

(** Return the smallest span that covers both inputs. *)
val union: t -> t -> t

(** Format a span for debugging. *)
val to_string: t -> string

(** Encode a span as JSON. *)
val to_json: t -> Data.Json.t
