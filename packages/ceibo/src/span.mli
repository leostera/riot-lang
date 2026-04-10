(** Source-text spans and offset-based position helpers. *)
open Std

(** A range in source text.

    Spans are represented as raw offsets into the original source. *)
type t = {
  start: int;
  end_: int;
}

(** Create a span from two offsets. *)
val make: start:int -> end_:int -> t

(** Return the width of the span in offsets.

    Use this when you need to measure how much source text the span covers.

    Example:
    ```ocaml
    let span = Span.make ~start:4 ~end_:9 in
    Span.length span = 5
    ```
*)
val length: t -> int

(** Return `true` if the span contains the given offset.

    Use this when mapping raw source offsets back into syntax nodes or tokens.

    Example:
    ```ocaml
    let span = Span.make ~start:4 ~end_:9 in

    Span.contains span 6 = true;
    Span.contains span 12 = false
    ```
*)
val contains: t -> int -> bool

(** Return `true` if the two spans overlap, and `false` when they are
    disjoint.

    Use this when checking whether two syntax elements cover any of the same
    source text.

    Example:
    ```ocaml
    let span1 = Span.make ~start:0 ~end_:5 in
    let span2 = Span.make ~start:3 ~end_:8 in
    let span3 = Span.make ~start:8 ~end_:12 in

    Span.overlaps span1 span2 = true;
    Span.overlaps span1 span3 = false
    ```
*)
val overlaps: t -> t -> bool

(** Return the smallest span that covers both inputs.

    Use this when building a parent span from two child spans.

    Example:
    ```ocaml
    let left = Span.make ~start:2 ~end_:4 in
    let right = Span.make ~start:6 ~end_:9 in

    Span.union left right = Span.make ~start:2 ~end_:9
    ```
*)
val union: t -> t -> t

(** Format a span for debugging.

    Example:
    ```ocaml
    let span = Span.make ~start:2 ~end_:9 in
    Span.to_string span = "2..9"
    ```
*)
val to_string: t -> string

(** Encode a span as JSON.

    Use this when serializing diagnostics or syntax metadata.
*)
val to_json: t -> Data.Json.t
