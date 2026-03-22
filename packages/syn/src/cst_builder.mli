open Std

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

val create_from_ceibo :
  Cst.green_node -> (Cst.source_file, error) result
