(** Source-text spans and offset-based position helpers. *)
open Std

(** A range in source text.

    Spans are represented as raw offsets into the original source. *)
type t = {
  start: int;
  end_: int;
}

(** Create a span from two offsets. *)
val make:
  (** Start offset. *)
  start:int ->
  (** End offset. *)
  end_:int ->
  t

(** Return the width of the span in offsets. *)
val length: t -> int

(** Return `true` if the span contains the given offset. *)
val contains:
  t ->
  (** Offset to test. *)
  int ->
  bool

(** Return `true` if the two spans overlap. *)
val overlaps: t -> t -> bool

(** Return the smallest span that covers both inputs. *)
val union: t -> t -> t

(** Format a span for debugging. *)
val to_string: t -> string

(** Encode a span as JSON. *)
val to_json: t -> Data.Json.t
