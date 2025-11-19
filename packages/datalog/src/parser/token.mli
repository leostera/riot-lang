type t =
  | Eof
  | Dot
  | Comma
  | LParen
  | RParen
  | Bang
  | ColonDash
  | Ident of string
  | Variable of string
  | String of { value : string; terminated : bool }
  | Integer of int
  | Wildcard
  | Comment of string
  | Whitespace
  | Gt
  | Lt
  | GtEq
  | LtEq
  | Eq
  | NotEq
  | Unknown of char

type located = { kind : t; span : Ceibo.Span.t }

val kind_name : t -> string
val to_string : t -> string
