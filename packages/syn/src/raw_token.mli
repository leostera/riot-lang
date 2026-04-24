open Std
open Std.Collections

type t = {
  kind: Syntax_kind2.t;
  span: Ceibo.Span.t;
  legacy_kind: Token.token_kind;
  has_newline: bool;
}
type stream = {
  raw: t Vector.t;
  significant: int Vector.t;
}
val create_stream: unit -> stream

val create_stream_with_capacity: raw:int -> significant:int -> stream

val push: stream -> t -> int

val push_significant: stream -> t -> int

val of_lexer_tokens: source:IO.IoVec.IoSlice.t -> Token.t list -> stream

val is_trivia: t -> bool

val is_significant: t -> bool

val width: t -> int

val slice: source:IO.IoVec.IoSlice.t -> t -> IO.IoVec.IoSlice.t

val text_slice: source:IO.IoVec.IoSlice.t -> t -> string

val contains_char: source:IO.IoVec.IoSlice.t -> t -> char -> bool

val has_newline: t -> bool
