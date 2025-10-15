open Std

val compile : string -> (Syntax_kind.t, string) Ceibo.Green.node -> Html.t
