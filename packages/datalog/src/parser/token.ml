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
  | Ident s -> "ident(" ^ s ^ ")"
  | Variable s -> "var(" ^ s ^ ")"
  | String { value; _ } -> "string(" ^ String.escaped value ^ ")"
  | Integer i -> "int(" ^ string_of_int i ^ ")"
  | Wildcard -> "_"
  | Comment s -> "comment(" ^ s ^ ")"
  | Whitespace -> "whitespace"
  | Gt -> ">"
  | Lt -> "<"
  | GtEq -> ">="
  | LtEq -> "<="
  | Eq -> "="
  | NotEq -> "!="
  | Unknown c -> "unknown(" ^ String.make 1 c ^ ")"

let to_string t = kind_name t
