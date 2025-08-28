open Sexplib.Std

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
[@@deriving sexp]

type t =
  | And
  | At
  | Bang
  | Caret
  | CloseDelim of delimiter
  | Colon
  | Comma
  | Comment of string
  | Docstring of string
  | Dollar
  | Dot
  | EOF
  | Eq
  | Gt
  | Hash
  | Ident of Symbol.t
  | Keyword of keyword
  | Literal of literal_kind
  | Lt
  | Minus
  | OpenDelim of delimiter
  | Or
  | Percent
  | Plus
  | Question
  | Semi
  | Slash
  | Star
  | Tilde
  | Unknown
[@@deriving sexp]

and literal_kind = String of string [@@deriving sexp]

and delimiter =
  | Paren (* ( ... ) *)
  | Brace (* { ... } *)
  | Bracket (* [ ... ] *)
  | Keyword of string (* begin/struct/sig ... end *)
[@@deriving sexp]

type tokens = t list [@@deriving sexp]

let equal (a : t) b = a = b

(*************************************************************************************************)

let pp_token ppf token =
  let sexp = sexp_of_t token in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

let pp_tokens ppf tokens =
  let sexp = sexp_of_tokens tokens in
  Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

(*************************************************************************************************)

let is_open_delim_keyword str =
  match str with
  | "begin" | "struct" | "sig" | "object" | "class" -> true
  | _ -> false

let is_close_delim_keyword str = String.equal str "end"

let find_keyword str : keyword option =
  match str with
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
  | "open" -> Some Open
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
