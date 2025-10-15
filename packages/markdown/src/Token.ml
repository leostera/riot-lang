open Std

type t = { kind : Syntax_kind.t; span : Ceibo.Span.t }

let make kind span = { kind; span }
