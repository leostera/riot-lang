open Std

type t = {
  kind: Syntax_kind2.t;
  span: Ceibo.Span.t;
  legacy_kind: Token.token_kind option;
}
val of_lexer_tokens: Token.t list -> t array * int array

val is_trivia: t -> bool

val is_significant: t -> bool

val width: t -> int

val text: source:string -> t -> string
