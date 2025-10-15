open Std

val parse :
  source:string -> Token.t list -> (Syntax_kind.t, string) Ceibo.Green.node
