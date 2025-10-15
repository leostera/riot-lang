open Std

type t = { kind : Syntax_kind.t; span : Ceibo.Span.t }

val make : Syntax_kind.t -> Ceibo.Span.t -> t
