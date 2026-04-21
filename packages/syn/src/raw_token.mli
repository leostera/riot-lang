open Std
open Std.Collections

type t = {
  kind: Syntax_kind2.t;
  span: Ceibo.Span.t;
  legacy_kind: Token.token_kind option;
}

type stream = {
  raw: t Vector.t;
  significant: int Vector.t;
}

val create_stream: unit -> stream

val push: stream -> t -> int

val push_significant: stream -> t -> int

val of_lexer_tokens: Token.t list -> stream

val is_trivia: t -> bool

val is_significant: t -> bool

val width: t -> int

val text: source:string -> t -> string

