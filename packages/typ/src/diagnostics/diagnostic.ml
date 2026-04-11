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

let span_serializer =
  Serde.Ser.record
    (Serde.Ser.fields
       [
         Serde.Ser.field "start" Serde.Ser.int (fun (span: Syn.Ceibo.Span.t) -> span.start);
         Serde.Ser.field "end" Serde.Ser.int (fun (span: Syn.Ceibo.Span.t) -> span.end_);
       ])

let unsupported_syntax_serializer =
  Serde.Ser.record
    (Serde.Ser.fields
       [
         Serde.Ser.field "span" span_serializer (fun (value: unsupported_syntax) -> value.span);
         Serde.Ser.field "kind"
           (Serde.Ser.contramap Syn.SyntaxKind.to_string Serde.Ser.string)
           (fun (value: unsupported_syntax) -> value.kind);
         Serde.Ser.field "summary" Serde.Ser.string (fun (value: unsupported_syntax) -> value.summary);
       ])

let unsupported_type_serializer =
  Serde.Ser.record
    (Serde.Ser.fields
       [
         Serde.Ser.field "span" span_serializer (fun (value: unsupported_type) -> value.span);
         Serde.Ser.field "summary" Serde.Ser.string (fun (value: unsupported_type) -> value.summary);
       ])

let serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "UnsupportedSyntax" unsupported_syntax_serializer (function
        | UnsupportedSyntax value -> Some value
        | _ -> None);
      Serde.Ser.Variant.newtype "UnsupportedType" unsupported_type_serializer (function
        | UnsupportedType value -> Some value
        | _ -> None);
    ]
