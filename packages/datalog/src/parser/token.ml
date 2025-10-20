open Std

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

let kind_name = function
  | Eof -> "EOF"
  | Dot -> "."
  | Comma -> ","
  | LParen -> "("
  | RParen -> ")"
  | Bang -> "!"
  | ColonDash -> ":-"
  | Ident s -> format "ident(%s)" s
  | Variable s -> format "var(%s)" s
  | String { value; _ } -> format "string(%S)" value
  | Integer i -> format "int(%d)" i
  | Wildcard -> "_"
  | Comment s -> format "comment(%s)" s
  | Whitespace -> "whitespace"
  | Gt -> ">"
  | Lt -> "<"
  | GtEq -> ">="
  | LtEq -> "<="
  | Eq -> "="
  | NotEq -> "!="
  | Unknown c -> format "unknown(%c)" c

let to_string t = kind_name t
