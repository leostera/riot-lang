open Std

type t =
  | All
  | HasAttachment
  | From of string
  | To of string
  | Subject of string
  | Contains of string
  | And of t * t
  | Or of t * t
  | Maybe of t

val parse : string -> (t, string) Result.t
val matches : t -> Message.t -> bool
val matches_entry : t -> Mbox.entry -> bool
