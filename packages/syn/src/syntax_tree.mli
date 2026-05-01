open Std
open Std.Collections

(**
   Vector-backed lossless syntax tree.

   The tree stores nodes, significant token leaves, and child edges in separate
   vectors. Child arrays intentionally omit trivia. Each token leaf records the
   raw token range that belongs to that source position, so leading whitespace,
   comments, and docstrings remain recoverable without allocating trivia nodes.
*)

(**
   Significant token leaf in the tree.

   `raw_lo`/`raw_hi` are an exclusive range into `t.raw_tokens` and include
   leading trivia before the body token. `body_raw` points at the significant
   raw token that decides this leaf's syntax kind.
*)
type token_leaf = {
  kind: Syntax_kind.t;
  raw_lo: int;
  raw_hi: int;
  body_raw: int;
}
(**
   Parser-inserted placeholder for a token that was required by the grammar but
   absent from the source.
*)
type missing = {
  kind: Syntax_kind.t;
  offset: int;
}
(** Child edge stored in a node's contiguous child range. *)
type child =
  | Node of int
  | Token of int
  | Missing of missing
(**
   Syntax node metadata.

   `raw_lo`/`raw_hi` cover raw tokens, including trivia owned by descendant
   token leaves. `full_width` is the byte width of that raw range, while
   `token_width` counts only significant token text.
*)
type node = {
  kind: Syntax_kind.t;
  first_child: int;
  child_count: int;
  raw_lo: int;
  raw_hi: int;
  full_width: int;
  token_width: int;
}
(** Complete lossless tree and the source/token storage it views. *)
type t = {
  source: IO.IoVec.IoSlice.t;
  raw_tokens: Raw_token.t Vector.t;
  significant_tokens: int Vector.t;
  tokens: token_leaf Vector.t;
  nodes: node Vector.t;
  children: child Vector.t;
  root: int;
}
type tree = t

(**
   Streaming tree builder used by the parser.

   The builder supports `precede`, checkpoints, and restore so recursive parser
   functions can reshape already-emitted children without allocating an
   intermediate concrete event list.
*)
module Builder: sig
  type t
  type marker
  type completed
  type checkpoint

  val create:
    source:IO.IoVec.IoSlice.t ->
    token_stream:Raw_token.stream ->
    ?event_capacity:int ->
    ?diagnostic_capacity:int ->
    unit ->
    t

  val start_node: t -> marker

  val complete: t -> marker -> Syntax_kind.t -> completed

  val precede: t -> completed -> marker

  val token: t -> raw_index:int -> unit

  val missing: t -> kind:Syntax_kind.t -> offset:int -> unit

  val error: t -> Diagnostic.t -> unit

  val length: t -> int

  val checkpoint: t -> checkpoint

  val restore: t -> checkpoint -> unit

  val diagnostics: t -> Diagnostic.t Vector.t

  val finish: t -> tree
end

(**
   Build a tree from an explicit event buffer.

   This is retained for event-buffer based tests/tools. The main parser writes
   directly into `Builder`.
*)
val build: source:IO.IoVec.IoSlice.t -> token_stream:Raw_token.stream -> events:Event.Buffer.t -> t

val root: t -> node

val node: t -> int -> node

val token: t -> int -> token_leaf

val child: t -> int -> child

val child_at: t -> node -> int -> child option

val for_each_child: t -> node -> fn:(child -> unit) -> unit

val token_width: t -> token_leaf -> int

val node_token_width: t -> node -> int

val token_contains_char: t -> token_leaf -> char -> bool

val token_text_is: t -> token_leaf -> string -> bool

val token_has_newline: t -> token_leaf -> bool

(** Source slice for the significant body token. *)
val token_text_slice: t -> token_leaf -> IO.IoVec.IoSlice.t

val token_text: t -> token_leaf -> string

(** Materialize a raw-token range, including trivia. *)
val raw_range_text: t -> raw_lo:int -> raw_hi:int -> string

val node_text: t -> node -> string

val to_json: t -> Std.Data.Json.t
