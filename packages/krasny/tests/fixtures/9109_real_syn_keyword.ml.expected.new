open Std

type t =
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
  | Lnot
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

let of_string = function
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
  | "lnot" -> Some Lnot
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

let to_string = function
  | And -> "and"
  | As -> "as"
  | Asr -> "asr"
  | Assert -> "assert"
  | Begin -> "begin"
  | Class -> "class"
  | Constraint -> "constraint"
  | Do -> "do"
  | Done -> "done"
  | Downto -> "downto"
  | Else -> "else"
  | End -> "end"
  | Exception -> "exception"
  | External -> "external"
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | Function -> "function"
  | Functor -> "functor"
  | If -> "if"
  | In -> "in"
  | Include -> "include"
  | Inherit -> "inherit"
  | Initializer -> "initializer"
  | Land -> "land"
  | Lazy -> "lazy"
  | Let -> "let"
  | Lor -> "lor"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Lxor -> "lxor"
  | Lnot -> "lnot"
  | Match -> "match"
  | Method -> "method"
  | Mod -> "mod"
  | Module -> "module"
  | Mutable -> "mutable"
  | New -> "new"
  | Nonrec -> "nonrec"
  | Object -> "object"
  | Of -> "of"
  | Open -> "open"
  | Or -> "or"
  | Private -> "private"
  | Rec -> "rec"
  | Sig -> "sig"
  | Struct -> "struct"
  | Then -> "then"
  | To -> "to"
  | True -> "true"
  | Try -> "try"
  | Type -> "type"
  | Val -> "val"
  | Virtual -> "virtual"
  | When -> "when"
  | While -> "while"
  | With -> "with"

let is_opening = function
  | Begin | Struct | Sig | Object -> true
  | _ -> false

let is_closing = function
  | End -> true
  | _ -> false
