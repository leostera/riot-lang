open Std
module Syntax_kind = Syntax_kind
module Diagnostic = Diagnostic

val parse :
  string ->
  ((Syntax_kind.t, string) Ceibo.Green.node, Diagnostic.t list) Result.t

val parse_query :
  string ->
  ((Syntax_kind.t, string) Ceibo.Green.node, Diagnostic.t list) Result.t
