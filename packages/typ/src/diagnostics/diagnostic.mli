type unsupported_syntax = {
  span: Syn.Ceibo.Span.t;
  kind: Syn.SyntaxKind.t;
  summary: string;
}

type unsupported_type = {
  span: Syn.Ceibo.Span.t;
  summary: string;
}

type t =
  | UnsupportedSyntax of unsupported_syntax
  | UnsupportedType of unsupported_type

val serializer : t Serde.Ser.t
