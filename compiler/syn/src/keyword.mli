(**
   OCaml Keywords

   This module defines all OCaml reserved keywords and utilities for working
   with them.

   Keywords are language-level identifiers with special meaning that cannot be
   used as variable names or other identifiers.
*)
open Std

(**
   All OCaml keywords.

   This covers standard OCaml keywords from the language specification. Note
   that some keywords like `begin`, `struct`, and `sig` also serve as opening
   delimiters.
*)
type t =
  | And
  | As
  | Asr
  | Begin
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
  | Land
  | Let
  | Lor
  | Lsl
  | Lsr
  | Lxor
  | Lnot
  | Match
  | Mod
  | Module
  | Mutable
  | Nonrec
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
  | When
  | While
  | With

(**
   `from_string str` parses a keyword from a string.

   Returns `Some keyword` if the string is a valid keyword, `None` otherwise.

   Example: ```ocaml Keyword.from_string "let" = Some Let Keyword.from_string "foo"
   = None Keyword.from_string "if" = Some If ```
*)
val from_string: string -> t option

(**
   `to_string kw` converts a keyword to its string representation.

   This is the inverse of `from_string` for valid keywords.

   Example: ```ocaml Keyword.to_string Let = "let" Keyword.to_string If = "if"
   Keyword.to_string True = "true" ```
*)
val to_string: t -> string

(**
   `is_opening kw` checks if a keyword opens a block.

   Opening keywords are: `begin`, `struct`, and `sig`.

   These keywords must be matched with a corresponding `end` keyword.

   Example: ```ocaml Keyword.is_opening Begin = true Keyword.is_opening Struct
   = true Keyword.is_opening Let = false ```
*)
val is_opening: t -> bool

(**
   `is_closing kw` checks if a keyword closes a block.

   The only closing keyword is `end`, which matches `begin`, `struct`, and
   `sig`.

   Example: ```ocaml Keyword.is_closing End = true Keyword.is_closing Done =
   false ```
*)
val is_closing: t -> bool
