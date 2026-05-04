open Std
open Std.Collections

(**
   Raw token stream produced by the lexer before syntax tree compaction.

   Raw tokens include trivia and EOF. The `significant` vector stores indexes
   into `raw` for non-trivia tokens, allowing the parser to advance over only
   grammar-relevant tokens while still preserving all source text for spans,
   diagnostics, comments, and formatter views.
*)
type t = {
  kind: Syntax_kind.t;
  span: Span.t;
  legacy_kind: Token.token_kind;
  has_newline: bool;
}
type stream = {
  raw: t Vector.t;
  significant: int Vector.t;
}

val create_stream: unit -> stream

val create_stream_with_capacity: raw:int -> significant:int -> stream

(** Push a raw token and return its raw index. *)
val push: stream -> t -> int

(** Push a non-trivia token and record its raw index as significant. *)
val push_significant: stream -> t -> int

(**
   Convert lexer tokens to a raw stream. Whitespace trivia is collapsed to a
   structural whitespace token for formatter-facing views, while comment and
   docstring trivia keep their source spans.
*)
val from_lexer_tokens: source:IO.IoVec.IoSlice.t -> Token.t list -> stream

val is_trivia: t -> bool

val is_significant: t -> bool

val width: t -> int

(** Source slice for this raw token. *)
val slice: source:IO.IoVec.IoSlice.t -> t -> IO.IoVec.IoSlice.t

val text_slice: source:IO.IoVec.IoSlice.t -> t -> string

val contains_char: source:IO.IoVec.IoSlice.t -> t -> char -> bool

val has_newline: t -> bool
