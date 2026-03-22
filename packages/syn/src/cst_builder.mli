open Std

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

val create_from_ceibo :
  kind:[ `Implementation | `Interface ] ->
  Cst.green_node -> (Cst.t, error) result
