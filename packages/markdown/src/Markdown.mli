open Std
module Html : module type of Html

type t = (Syntax_kind.t, string) Ceibo.Green.node

val parse : string -> t
val compile : t -> Html.t
