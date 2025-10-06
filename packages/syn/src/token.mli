open Std

type keyword =
  | And
  | As
  | Asr
  | Assert
  | Begin
  | Class
  | Constraint
  | Do
  | Done
  | Downto
  | Else
  | End
  | Exception
  | External
  | False
  | For
  | Fun
  | Function
  | Functor
  | If
  | In
  | Include
  | Inherit
  | Initializer
  | Land
  | Lazy
  | Let
  | Lor
  | Lsl
  | Lsr
  | Lxor
  | Match
  | Method
  | Mod
  | Module
  | Mutable
  | New
  | Nonrec
  | Object
  | Of
  | Open
  | Or
  | Private
  | Rec
  | Sig
  | Struct
  | Then
  | To
  | True
  | Try
  | Type
  | Val
  | Virtual
  | When
  | While
  | With

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

type t =
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

val keyword_of_string : string -> keyword option
val is_opening_keyword : string -> bool
val is_closing_keyword : string -> bool
val delimiter_of_keyword : string -> delimiter option
