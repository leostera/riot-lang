open Std

(** OCaml keywords *)
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

val of_string : string -> t option
(** Parse a keyword from a string *)

val to_string : t -> string
(** Convert a keyword to its string representation *)

val is_opening : t -> bool
(** Check if a keyword opens a block (begin, struct, sig, object) *)

val is_closing : t -> bool
(** Check if a keyword closes a block (end) *)
