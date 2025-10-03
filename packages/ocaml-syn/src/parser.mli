open Std

type error =
  | UnexpectedToken of { expected : string; found : Token.t }
  | UnexpectedEOF
  | InvalidPattern
  | InvalidExpression of string

type t

val create : Token.t list -> t
val parse : t -> (Ast.structure, error) result
