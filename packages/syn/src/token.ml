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

let keyword_of_string : string -> keyword option = function
  | "and" -> Some And
  | "as" -> Some As
  | "asr" -> Some Asr
  | "assert" -> Some Assert
  | "begin" -> Some Begin
  | "class" -> Some Class
  | "constraint" -> Some Constraint
  | "do" -> Some Do
  | "done" -> Some Done
  | "downto" -> Some Downto
  | "else" -> Some Else
  | "end" -> Some End
  | "exception" -> Some Exception
  | "external" -> Some External
  | "false" -> Some False
  | "for" -> Some For
  | "fun" -> Some Fun
  | "function" -> Some Function
  | "functor" -> Some Functor
  | "if" -> Some If
  | "in" -> Some In
  | "include" -> Some Include
  | "inherit" -> Some Inherit
  | "initializer" -> Some Initializer
  | "land" -> Some Land
  | "lazy" -> Some Lazy
  | "let" -> Some Let
  | "lor" -> Some Lor
  | "lsl" -> Some Lsl
  | "lsr" -> Some Lsr
  | "lxor" -> Some Lxor
  | "match" -> Some Match
  | "method" -> Some Method
  | "mod" -> Some Mod
  | "module" -> Some Module
  | "mutable" -> Some Mutable
  | "new" -> Some New
  | "nonrec" -> Some Nonrec
  | "object" -> Some Object
  | "of" -> Some Of
  | "open" -> Some Open
  | "or" -> Some Or
  | "private" -> Some Private
  | "rec" -> Some Rec
  | "sig" -> Some Sig
  | "struct" -> Some Struct
  | "then" -> Some Then
  | "to" -> Some To
  | "true" -> Some True
  | "try" -> Some Try
  | "type" -> Some Type
  | "val" -> Some Val
  | "virtual" -> Some Virtual
  | "when" -> Some When
  | "while" -> Some While
  | "with" -> Some With
  | _ -> None

let is_opening_keyword = function
  | "begin" | "struct" | "sig" | "object" -> true
  | _ -> false

let is_closing_keyword = function "end" -> true | _ -> false

let delimiter_of_keyword : string -> delimiter option = function
  | "begin" -> Some BeginEnd
  | "struct" -> Some StructEnd
  | "sig" -> Some SigEnd
  | "object" -> Some ObjectEnd
  | _ -> None
