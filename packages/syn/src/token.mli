open Std

type keyword = Keyword.t

type literal =
  | String of { value : string; terminated : bool }
  | Int of int
  | Float of float
  | Char of char

type delimiter =
  | Paren
  | Brace
  | Bracket
  | BeginEnd
  | StructEnd
  | SigEnd
  | ObjectEnd

type token_kind =
  | Keyword of keyword
  | Ident of string
  | Literal of literal
  | OpenDelim of delimiter
  | CloseDelim of delimiter
  | Comment of { value : string; terminated : bool }
  | Docstring of { value : string; terminated : bool }
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Eq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Ne
  | Bang
  | And
  | Or
  | Colon
  | Semi
  | Comma
  | Dot
  | Arrow
  | FatArrow
  | ColonColon
  | ColonEq
  | Question
  | At
  | Hash
  | Tilde
  | Dollar
  | Pipe
  | Ampersand
  | Underscore
  | Whitespace
  | EOF
  | Unknown of char

type t = { kind : token_kind; span : Ceibo.Span.t }

val delimiter_of_keyword : keyword -> delimiter option
(** Get the delimiter associated with a keyword (begin, struct, sig, object) *)

val show_kind : token_kind -> string
(** Get a human-readable name for a token kind *)
