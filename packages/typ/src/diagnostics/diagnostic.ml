open Std

type unsupported_syntax = {
  span: Syn.Span.t;
  kind: Syn.SyntaxKind.t;
  summary: string;
}

type unsupported_type = {
  span: Syn.Span.t;
  summary: string;
}

type annotation_mismatch = {
  span: Syn.Span.t;
  annotation_span: Syn.Span.t;
  expected: string;
  actual: string;
}

type infinite_substitution = {
  span: Syn.Span.t;
  var: string;
  type_: string;
}

type type_mismatch = {
  span: Syn.Span.t;
  expected: string;
  actual: string;
}

type unerasable_optional_argument = {
  span: Syn.Span.t;
  label: string;
}

type severity =
  | Error
  | Warning

type t =
  | UnsupportedSyntax of unsupported_syntax
  | UnsupportedType of unsupported_type
  | AnnotationMismatch of annotation_mismatch
  | InfiniteSubstitution of infinite_substitution
  | TypeMismatch of type_mismatch
  | UnerasableOptionalArgument of unerasable_optional_argument

let annotation_mismatch ~span ~annotation_span ~expected ~actual =
  AnnotationMismatch { span; annotation_span; expected; actual }

let infinite_substitution ~span ~var ~type_ =
  InfiniteSubstitution { span; var; type_ }

let type_mismatch ~span ~expected ~actual =
  TypeMismatch { span; expected; actual }

let unerasable_optional_argument ~span ~label =
  UnerasableOptionalArgument { span; label }

let id diagnostic =
  match diagnostic with
  | UnsupportedSyntax _ -> Error.T0001_UnsupportedSyntax
  | UnsupportedType _ -> Error.T0002_UnsupportedType
  | AnnotationMismatch _ -> Error.T0003_AnnotationMismatch
  | InfiniteSubstitution _ -> Error.T0004_InfiniteSubstitution
  | TypeMismatch _ -> Error.T0005_TypeMismatch
  | UnerasableOptionalArgument _ -> Error.T0006_UnerasableOptionalArgument

let span diagnostic =
  match diagnostic with
  | UnsupportedSyntax { span; _ }
  | UnsupportedType { span; _ }
  | AnnotationMismatch { span; _ }
  | InfiniteSubstitution { span; _ }
  | TypeMismatch { span; _ }
  | UnerasableOptionalArgument { span; _ } -> span

let severity diagnostic =
  match diagnostic with
  | UnsupportedSyntax _
  | UnsupportedType _
  | AnnotationMismatch _
  | InfiniteSubstitution _
  | TypeMismatch _ -> Error
  | UnerasableOptionalArgument _ -> Warning

let hint diagnostic =
  diagnostic
  |> id
  |> Error.explain

let fix diagnostic =
  match diagnostic with
  | UnerasableOptionalArgument _ ->
      Some "consider adding a `()` positional argument as your last function argument"
  | UnsupportedSyntax _
  | UnsupportedType _
  | AnnotationMismatch _
  | InfiniteSubstitution _
  | TypeMismatch _ -> None

let to_string diagnostic =
  match diagnostic with
  | UnsupportedSyntax { summary; _ } -> "Unsupported syntax: " ^ summary
  | UnsupportedType { summary; _ } -> "Unsupported type: " ^ summary
  | AnnotationMismatch { expected; actual; _ } ->
      "This annotation expects "
      ^ expected
      ^ " but the value has type "
      ^ actual
  | InfiniteSubstitution { var; type_; _ } ->
      "Type variable "
      ^ var
      ^ " cannot be substituted with "
      ^ type_
  | TypeMismatch { expected; actual; _ } ->
      "Expected "
      ^ expected
      ^ " but got "
      ^ actual
  | UnerasableOptionalArgument { label; _ } ->
      "Optional argument ?"
      ^ label
      ^ " cannot be erased"

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"

let span_to_json = fun (span: Syn.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let to_json = fun diagnostic ->
  let message = to_string diagnostic in
  let hint = hint diagnostic in
  let open Data.Json in
  let error_id = id diagnostic in
  let details =
    match diagnostic with
    | UnsupportedSyntax { kind; summary; _ } ->
        [ ("syntax_kind", String (Syn.SyntaxKind.to_string kind)); ("summary", String summary); ]
    | UnsupportedType { summary; _ } -> [ ("summary", String summary); ]
    | AnnotationMismatch { annotation_span; expected; actual; _ } ->
        [
          ("annotation_span", span_to_json annotation_span);
          ("expected", String expected);
          ("actual", String actual);
        ]
    | InfiniteSubstitution { var; type_; _ } ->
        [ ("var", String var); ("type", String type_); ]
    | TypeMismatch { expected; actual; _ } ->
        [ ("expected", String expected); ("actual", String actual); ]
    | UnerasableOptionalArgument { label; _ } -> [ ("label", String label); ]
  in
  let fix_fields =
    match fix diagnostic with
    | Some fix -> [ ("fix", String fix); ]
    | None -> []
  in
  Object [
    (
      "kind",
      Object (
        [
          ("id", String (Error.id_to_string error_id));
          ("name", String (Error.name error_id));
          ("severity", String (severity_to_string (severity diagnostic)));
          ("message", String message);
        ]
        @ details
        @ fix_fields
        @ [ ("hint", String hint); ]
      )
    );
    ("span", span_to_json (span diagnostic));
  ]

let span_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "start" Serde.Ser.int (fun (span: Syn.Span.t) -> span.start);
      Serde.Ser.field "end" Serde.Ser.int (fun (span: Syn.Span.t) -> span.end_);
    ])

let unsupported_syntax_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: unsupported_syntax) -> value.span);
      Serde.Ser.field
        "kind"
        (Serde.Ser.contramap Syn.SyntaxKind.to_string Serde.Ser.string)
        (fun (value: unsupported_syntax) -> value.kind);
      Serde.Ser.field "summary" Serde.Ser.string (fun (value: unsupported_syntax) -> value.summary);
    ])

let unsupported_type_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: unsupported_type) -> value.span);
      Serde.Ser.field "summary" Serde.Ser.string (fun (value: unsupported_type) -> value.summary);
    ])

let annotation_mismatch_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: annotation_mismatch) -> value.span);
      Serde.Ser.field "annotation_span" span_serializer (fun (value: annotation_mismatch) -> value.annotation_span);
      Serde.Ser.field "expected" Serde.Ser.string (fun (value: annotation_mismatch) -> value.expected);
      Serde.Ser.field "actual" Serde.Ser.string (fun (value: annotation_mismatch) -> value.actual);
    ])

let infinite_substitution_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: infinite_substitution) -> value.span);
      Serde.Ser.field "var" Serde.Ser.string (fun (value: infinite_substitution) -> value.var);
      Serde.Ser.field "type" Serde.Ser.string (fun (value: infinite_substitution) -> value.type_);
    ])

let type_mismatch_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: type_mismatch) -> value.span);
      Serde.Ser.field "expected" Serde.Ser.string (fun (value: type_mismatch) -> value.expected);
      Serde.Ser.field "actual" Serde.Ser.string (fun (value: type_mismatch) -> value.actual);
    ])

let unerasable_optional_argument_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "span" span_serializer (fun (value: unerasable_optional_argument) -> value.span);
      Serde.Ser.field "label" Serde.Ser.string (fun (value: unerasable_optional_argument) -> value.label);
    ])

let serializer = Serde.Ser.variant
  [ Serde.Ser.Variant.newtype "UnsupportedSyntax" unsupported_syntax_serializer
      (
        function
        | UnsupportedSyntax value -> Some value
        | _ -> None
      ); Serde.Ser.Variant.newtype "UnsupportedType" unsupported_type_serializer
      (
        function
        | UnsupportedType value -> Some value
        | _ -> None
      ); Serde.Ser.Variant.newtype "AnnotationMismatch" annotation_mismatch_serializer
      (
        function
        | AnnotationMismatch value -> Some value
        | _ -> None
      ); Serde.Ser.Variant.newtype "InfiniteSubstitution" infinite_substitution_serializer
      (
        function
        | InfiniteSubstitution value -> Some value
        | _ -> None
      ); Serde.Ser.Variant.newtype "TypeMismatch" type_mismatch_serializer
      (
        function
        | TypeMismatch value -> Some value
        | _ -> None
      ); Serde.Ser.Variant.newtype "UnerasableOptionalArgument" unerasable_optional_argument_serializer
      (
        function
        | UnerasableOptionalArgument value -> Some value
        | _ -> None
      ); ]
