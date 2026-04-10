open Std

type severity =
  | Error
  | Warning

type supported_literal =
  | IntLiteral
  | FloatLiteral
  | BoolLiteral
  | StringLiteral
  | CharLiteral
  | UnitLiteral

type unsupported_reason =
  | LiteralOutsideSupportedSubset of { supported_literals: supported_literal list }

type unsupported_context =
  | StructureItem
  | SignatureItem
  | Pattern
  | Expression

type unsupported_recovery =
  | PlaceholderItem
  | RecoveryPattern
  | HoleExpression

type mismatch =
  | ExpectedActual of { expected: string; actual: string }
  | TupleArityMismatch of { left: string; right: string; left_arity: int; right_arity: int }
  | OccursCheckFailed of { variable_id: int; in_type: string }

type application_label =
  | PositionalArgument
  | LabeledArgument of string
  | OptionalArgument of string

type record_context =
  | RecordConstruction
  | RecordUpdate
  | RecordPattern
  | RecordFieldAccess

type record_resolution_reason =
  | UnknownRecordLabels of string list
  | AmbiguousRecordLabels of string list
  | MissingRecordFields of string list
  | IncompatibleRecordLabels of string list

type signature_mismatch =
  | MissingValue of { name: string }
  | ValueTypeMismatch of { name: string; expected: string; actual: string }
  | MissingTypeDeclaration of { name: string }
  | TypeDeclarationMismatch of { name: string; expected: string; actual: string }

type match_witness =
  | WildcardWitness
  | BoolWitness of bool
  | UnitWitness
  | TupleWitness of match_witness list
  | ConstructorWitness of { name: string; arguments: match_witness list }

type t =
  | CstBuilderError of { builder_error: Syn.CstBuilder.error }
  | UnsupportedSyntax of {
      syntax_span: Syn.Ceibo.Span.t;
      syntax_kind: Syn.SyntaxKind.t;
      context: unsupported_context;
      recovery: unsupported_recovery;
      reason: unsupported_reason option
    }
  | IgnoredPatternTypeConstraint of { constraint_span: Syn.Ceibo.Span.t }
  | ParameterLoweredAsPositional of { parameter_span: Syn.Ceibo.Span.t }
  | ApplicationArgumentLoweredAsPositional of { application_span: Syn.Ceibo.Span.t }
  | IgnoredTypeAscription of { ascription_span: Syn.Ceibo.Span.t }
  | IgnoredPolymorphicAnnotation of { annotation_span: Syn.Ceibo.Span.t }
  | UnsupportedInterfaceFile of { interface_span: Syn.Ceibo.Span.t }
  | UnboundName of { reference_span: Syn.Ceibo.Span.t; name: string }
  | TypeMismatch of { mismatch_span: Syn.Ceibo.Span.t; mismatch: mismatch }
  | ApplicationLabelMismatch of {
      application_span: Syn.Ceibo.Span.t;
      expected_label: application_label;
      actual_labels: application_label list
    }
  | RecordResolutionError of {
      operation_span: Syn.Ceibo.Span.t;
      context: record_context;
      reason: record_resolution_reason
    }
  | OrPatternBindingsMismatch of {
      pattern_span: Syn.Ceibo.Span.t;
      expected_names: string list;
      actual_names: string list
    }
  | SignatureInclusionError of {
      mismatch_span: Syn.Ceibo.Span.t;
      counterpart_span: Syn.Ceibo.Span.t option;
      mismatch: signature_mismatch
    }
  | NonexhaustiveMatch of { match_span: Syn.Ceibo.Span.t; witness: match_witness }
  | RedundantMatchCase of { case_span: Syn.Ceibo.Span.t }
  | UnsupportedSemanticExpression of { expression_span: Syn.Ceibo.Span.t; summary: string }
  | RecursiveGroupRequiresSimpleVariableBinders of { binding_span: Syn.Ceibo.Span.t }

let code = fun value ->
  match value with
  | CstBuilderError _ -> "TYP1011"
  | UnsupportedSyntax _ -> "TYP1001"
  | IgnoredPatternTypeConstraint _ -> "TYP1004"
  | ParameterLoweredAsPositional _ -> "TYP1005"
  | ApplicationArgumentLoweredAsPositional _ -> "TYP1007"
  | IgnoredTypeAscription _ -> "TYP1008"
  | IgnoredPolymorphicAnnotation _ -> "TYP1009"
  | UnsupportedInterfaceFile _ -> "TYP1010"
  | UnboundName _ -> "TYP2001"
  | TypeMismatch _ -> "TYP2002"
  | ApplicationLabelMismatch _ -> "TYP2005"
  | RecordResolutionError _ -> "TYP2006"
  | OrPatternBindingsMismatch _ -> "TYP2007"
  | NonexhaustiveMatch _ -> "TYP1012"
  | RedundantMatchCase _ -> "TYP1013"
  | SignatureInclusionError _ -> "TYP2011"
  | UnsupportedSemanticExpression _ -> "TYP2010"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "TYP2004"

let name = fun value ->
  match value with
  | CstBuilderError _ -> "cst-builder-error"
  | UnsupportedSyntax _ -> "unsupported-syntax"
  | IgnoredPatternTypeConstraint _ -> "ignored-pattern-type-constraint"
  | ParameterLoweredAsPositional _ -> "parameter-lowered-as-positional"
  | ApplicationArgumentLoweredAsPositional _ -> "application-argument-lowered-as-positional"
  | IgnoredTypeAscription _ -> "ignored-type-ascription"
  | IgnoredPolymorphicAnnotation _ -> "ignored-polymorphic-annotation"
  | UnsupportedInterfaceFile _ -> "unsupported-interface-file"
  | UnboundName _ -> "unbound-name"
  | TypeMismatch _ -> "type-mismatch"
  | ApplicationLabelMismatch _ -> "application-label-mismatch"
  | RecordResolutionError _ -> "record-resolution-error"
  | OrPatternBindingsMismatch _ -> "or-pattern-bindings-mismatch"
  | NonexhaustiveMatch _ -> "nonexhaustive-match"
  | RedundantMatchCase _ -> "redundant-match-case"
  | SignatureInclusionError _ -> "signature-inclusion-error"
  | UnsupportedSemanticExpression _ -> "unsupported-semantic-expression"
  | RecursiveGroupRequiresSimpleVariableBinders _ -> "recursive-group-requires-simple-variable-binders"

let severity = fun value ->
  match value with
  | CstBuilderError _
  | UnsupportedSyntax _
  | IgnoredPatternTypeConstraint _
  | IgnoredPolymorphicAnnotation _
  | UnboundName _
  | TypeMismatch _
  | ApplicationLabelMismatch _
  | RecordResolutionError _
  | OrPatternBindingsMismatch _
  | SignatureInclusionError _
  | UnsupportedSemanticExpression _
  | RecursiveGroupRequiresSimpleVariableBinders _ -> Error
  | ParameterLoweredAsPositional _
  | ApplicationArgumentLoweredAsPositional _
  | IgnoredTypeAscription _
  | UnsupportedInterfaceFile _
  | NonexhaustiveMatch _
  | RedundantMatchCase _ -> Warning

let severity_to_string = fun value ->
  match value with
  | Error -> "error"
  | Warning -> "warning"

let primary_span = fun value ->
  match value with
  | CstBuilderError { builder_error } -> builder_error.span
  | UnsupportedSyntax { syntax_span; _ } -> syntax_span
  | IgnoredPatternTypeConstraint { constraint_span } -> constraint_span
  | ParameterLoweredAsPositional { parameter_span } -> parameter_span
  | ApplicationArgumentLoweredAsPositional { application_span } -> application_span
  | IgnoredTypeAscription { ascription_span } -> ascription_span
  | IgnoredPolymorphicAnnotation { annotation_span } -> annotation_span
  | UnsupportedInterfaceFile { interface_span } -> interface_span
  | UnboundName { reference_span; _ } -> reference_span
  | TypeMismatch { mismatch_span; _ } -> mismatch_span
  | ApplicationLabelMismatch { application_span; _ } -> application_span
  | RecordResolutionError { operation_span; _ } -> operation_span
  | OrPatternBindingsMismatch { pattern_span; _ } -> pattern_span
  | NonexhaustiveMatch { match_span; _ } -> match_span
  | RedundantMatchCase { case_span } -> case_span
  | SignatureInclusionError { mismatch_span; _ } -> mismatch_span
  | UnsupportedSemanticExpression { expression_span; _ } -> expression_span
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } -> binding_span

let supported_literal_to_string = fun value ->
  match value with
  | IntLiteral -> "int"
  | FloatLiteral -> "float"
  | BoolLiteral -> "bool"
  | StringLiteral -> "string"
  | CharLiteral -> "char"
  | UnitLiteral -> "unit"

let unsupported_context_tag = fun value ->
  match value with
  | StructureItem -> "structure_item"
  | SignatureItem -> "signature_item"
  | Pattern -> "pattern"
  | Expression -> "expression"

let unsupported_context_to_string = fun value ->
  match value with
  | StructureItem -> "structure item"
  | SignatureItem -> "signature item"
  | Pattern -> "pattern"
  | Expression -> "expression"

let unsupported_recovery_tag = fun value ->
  match value with
  | PlaceholderItem -> "placeholder_item"
  | RecoveryPattern -> "recovery_pattern"
  | HoleExpression -> "hole_expression"

let unsupported_recovery_to_string = fun value ->
  match value with
  | PlaceholderItem -> "placeholder item"
  | RecoveryPattern -> "recovery pattern"
  | HoleExpression -> "type hole"

let unsupported_reason_to_string = fun value ->
  match value with
  | LiteralOutsideSupportedSubset { supported_literals } -> format
    Format.[
      str "literal kind is outside the currently supported subset (supported: ";
      str (String.concat ", " (List.map supported_literal_to_string supported_literals));
      str ")";
    ]

let application_label_to_string = fun value ->
  match value with
  | PositionalArgument -> "positional"
  | LabeledArgument label -> format Format.[ str "~"; str label ]
  | OptionalArgument label -> format Format.[ str "?"; str label ]

let record_context_to_string = fun value ->
  match value with
  | RecordConstruction -> "record construction"
  | RecordUpdate -> "record update"
  | RecordPattern -> "record pattern"
  | RecordFieldAccess -> "record field access"

let render_record_labels = fun labels ->
  String.concat ", " labels

let render_binding_names = fun names ->
  match names with
  | [] -> "(none)"
  | _ -> render_record_labels names

let signature_mismatch_name = fun value ->
  match value with
  | MissingValue { name } -> format Format.[ str "value "; str name ]
  | ValueTypeMismatch { name; _ } -> format Format.[ str "value "; str name ]
  | MissingTypeDeclaration { name } -> format Format.[ str "type "; str name ]
  | TypeDeclarationMismatch { name; _ } -> format Format.[ str "type "; str name ]

let signature_mismatch_message = fun value ->
  match value with
  | MissingValue { name } -> format
    Format.[ str "signature inclusion failed: implementation does not export value "; str name ]
  | ValueTypeMismatch { name; expected; actual } -> format
    Format.[
      str "signature inclusion failed: value ";
      str name;
      str " has type ";
      str actual;
      str " but the interface requires ";
      str expected
    ]
  | MissingTypeDeclaration { name } -> format
    Format.[ str "signature inclusion failed: implementation does not export type "; str name ]
  | TypeDeclarationMismatch { name; expected; actual } -> format
    Format.[
      str "signature inclusion failed: type ";
      str name;
      str " does not match the interface (expected ";
      str expected;
      str ", got ";
      str actual;
      str ")"
    ]

let rec match_witness_to_string = fun value ->
  match value with
  | WildcardWitness -> "_"
  | BoolWitness value -> Bool.to_string value
  | UnitWitness -> "()"
  | TupleWitness elements -> format
    Format.[ str "("; str (String.concat ", " (List.map match_witness_to_string elements)); str ")" ]
  | ConstructorWitness { name="::"; arguments=[head;tail] } -> format
    Format.[
      str (match_witness_atom_to_string head);
      str " :: ";
      str (match_witness_atom_to_string tail);
    ]
  | ConstructorWitness { name; arguments=[] } -> name
  | ConstructorWitness { name; arguments=[ argument ] } -> format
    Format.[ str name; str " "; str (match_witness_atom_to_string argument) ]
  | ConstructorWitness { name; arguments } -> format
    Format.[
      str name;
      str " (";
      str (String.concat ", " (List.map match_witness_to_string arguments));
      str ")"
    ]

and match_witness_atom_to_string = function
  | WildcardWitness as witness -> match_witness_to_string witness
  | BoolWitness _ as witness -> match_witness_to_string witness
  | UnitWitness as witness -> match_witness_to_string witness
  | ConstructorWitness { arguments=[]; _ } as witness -> match_witness_to_string witness
  | witness -> "(" ^ match_witness_to_string witness ^ ")"

let message = fun value ->
  match value with
  | CstBuilderError { builder_error } ->
      format Format.[ str "Syn.build_cst failed before lowering: "; str (builder_error.message) ]
  | UnsupportedSyntax {
    syntax_kind;
    context;
    recovery;
    reason=None;
    _
  } ->
      format
        Format.[
          str "unsupported ";
          str (unsupported_context_to_string context);
          str " lowered using ";
          str (unsupported_recovery_to_string recovery);
          str ": ";
          str (Syn.SyntaxKind.to_string syntax_kind);
        ]
  | UnsupportedSyntax {
    syntax_kind;
    context;
    recovery;
    reason=Some reason;
    _
  } ->
      format
        Format.[
          str "unsupported ";
          str (unsupported_context_to_string context);
          str " lowered using ";
          str (unsupported_recovery_to_string recovery);
          str ": ";
          str (Syn.SyntaxKind.to_string syntax_kind);
          str " (";
          str (unsupported_reason_to_string reason);
          str ")";
        ]
  | IgnoredPatternTypeConstraint _ ->
      "type-constrained pattern lowered without its annotation"
  | ParameterLoweredAsPositional _ ->
      "labeled, optional, or locally abstract parameters are currently lowered as ordinary positional binders"
  | ApplicationArgumentLoweredAsPositional _ ->
      "labeled or optional application arguments are currently lowered as ordinary positional arguments"
  | IgnoredTypeAscription _ ->
      "type ascriptions are currently ignored during lowering"
  | IgnoredPolymorphicAnnotation _ ->
      "explicit polymorphic annotations are currently ignored during lowering"
  | UnsupportedInterfaceFile _ ->
      "interface files are not lowered by the prototype yet"
  | UnboundName { name; _ } ->
      format Format.[ str "unbound name: "; str name ]
  | TypeMismatch { mismatch=ExpectedActual { expected; actual }; _ } ->
      format Format.[ str "type mismatch: expected "; str expected; str " but got "; str actual ]
  | TypeMismatch { mismatch=TupleArityMismatch { left; right; left_arity; right_arity }; _ } ->
      format
        Format.[
          str "type mismatch: tuple arity mismatch (";
          int left_arity;
          str " vs ";
          int right_arity;
          str ") between ";
          str left;
          str " and ";
          str right
        ]
  | TypeMismatch { mismatch=OccursCheckFailed { variable_id; in_type }; _ } ->
      format
        Format.[
          str "type mismatch: occurs check failed for type variable ";
          int variable_id;
          str " in ";
          str in_type
        ]
  | ApplicationLabelMismatch { expected_label; actual_labels=[]; _ } ->
      format
        Format.[
          str "application is missing an argument for ";
          str (application_label_to_string expected_label)
        ]
  | ApplicationLabelMismatch { expected_label; actual_labels; _ } ->
      format
        Format.[
          str "application label mismatch: expected ";
          str (application_label_to_string expected_label);
          str " but remaining arguments were ";
          str (String.concat ", " (List.map application_label_to_string actual_labels));
        ]
  | RecordResolutionError { context; reason; _ } -> (
      match reason with
      | UnknownRecordLabels labels -> format
        Format.[
          str (record_context_to_string context);
          str " uses unknown record labels: ";
          str (render_record_labels labels)
        ]
      | AmbiguousRecordLabels labels -> format
        Format.[
          str (record_context_to_string context);
          str " is ambiguous for record labels: ";
          str (render_record_labels labels)
        ]
      | MissingRecordFields labels -> format
        Format.[
          str (record_context_to_string context);
          str " is missing record fields: ";
          str (render_record_labels labels)
        ]
      | IncompatibleRecordLabels labels -> format
        Format.[
          str (record_context_to_string context);
          str " labels do not belong to a single record type: ";
          str (render_record_labels labels)
        ]
    )
  | OrPatternBindingsMismatch { expected_names; actual_names; _ } ->
      format
        Format.[
          str "or-pattern alternatives must bind the same names (expected: ";
          str (render_binding_names expected_names);
          str "; actual: ";
          str (render_binding_names actual_names);
          str ")";
        ]
  | NonexhaustiveMatch { witness; _ } ->
      format
        Format.[ str "non-exhaustive match: missing case "; str (match_witness_to_string witness) ]
  | RedundantMatchCase _ ->
      "match case is redundant"
  | SignatureInclusionError { mismatch; _ } ->
      signature_mismatch_message mismatch
  | UnsupportedSemanticExpression { summary; _ } ->
      format Format.[ str "unsupported semantic expression reached inference: "; str summary ]
  | RecursiveGroupRequiresSimpleVariableBinders _ ->
      "recursive groups currently require simple function bindings"

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let mismatch_to_json = fun value ->
  match value with
  | ExpectedActual { expected; actual } -> Data.Json.Object [
    ("tag", Data.Json.String "expected_actual");
    ("expected", Data.Json.String expected);
    ("actual", Data.Json.String actual);
  ]
  | TupleArityMismatch { left; right; left_arity; right_arity } -> Data.Json.Object [
    ("tag", Data.Json.String "tuple_arity_mismatch");
    ("left", Data.Json.String left);
    ("right", Data.Json.String right);
    ("left_arity", Data.Json.Int left_arity);
    ("right_arity", Data.Json.Int right_arity);
  ]
  | OccursCheckFailed { variable_id; in_type } -> Data.Json.Object [
    ("tag", Data.Json.String "occurs_check_failed");
    ("variable_id", Data.Json.Int variable_id);
    ("in_type", Data.Json.String in_type);
  ]

let supported_literal_to_json = fun literal -> Data.Json.String (supported_literal_to_string literal)

let application_label_to_json = fun value ->
  match value with
  | PositionalArgument -> Data.Json.Object [ ("tag", Data.Json.String "positional"); ]
  | LabeledArgument label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | OptionalArgument label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let record_context_to_json = fun value ->
  match value with
  | RecordConstruction -> Data.Json.String "construction"
  | RecordUpdate -> Data.Json.String "update"
  | RecordPattern -> Data.Json.String "pattern"
  | RecordFieldAccess -> Data.Json.String "field_access"

let record_resolution_reason_to_json = fun value ->
  match value with
  | UnknownRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "unknown_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | AmbiguousRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "ambiguous_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | MissingRecordFields labels -> Data.Json.Object [
    ("tag", Data.Json.String "missing_fields");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]
  | IncompatibleRecordLabels labels -> Data.Json.Object [
    ("tag", Data.Json.String "incompatible_labels");
    ("labels", Data.Json.Array (List.map (fun label -> Data.Json.String label) labels));
  ]

let names_to_json = fun names -> Data.Json.Array (List.map (fun name -> Data.Json.String name) names)

let rec match_witness_to_json = fun value ->
  match value with
  | WildcardWitness -> Data.Json.Object [ ("tag", Data.Json.String "wildcard") ]
  | BoolWitness value -> Data.Json.Object [
    ("tag", Data.Json.String "bool");
    ("value", Data.Json.Bool value);
  ]
  | UnitWitness -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | TupleWitness elements -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    ("elements", Data.Json.Array (List.map match_witness_to_json elements));
  ]
  | ConstructorWitness { name; arguments } -> Data.Json.Object [
    ("tag", Data.Json.String "constructor");
    ("name", Data.Json.String name);
    ("arguments", Data.Json.Array (List.map match_witness_to_json arguments));
  ]

let signature_mismatch_to_json = fun value ->
  match value with
  | MissingValue { name } -> Data.Json.Object [
    ("tag", Data.Json.String "missing_value");
    ("name", Data.Json.String name);
  ]
  | ValueTypeMismatch { name; expected; actual } -> Data.Json.Object [
    ("tag", Data.Json.String "value_type_mismatch");
    ("name", Data.Json.String name);
    ("expected", Data.Json.String expected);
    ("actual", Data.Json.String actual);
  ]
  | MissingTypeDeclaration { name } -> Data.Json.Object [
    ("tag", Data.Json.String "missing_type_declaration");
    ("name", Data.Json.String name);
  ]
  | TypeDeclarationMismatch { name; expected; actual } -> Data.Json.Object [
    ("tag", Data.Json.String "type_declaration_mismatch");
    ("name", Data.Json.String name);
    ("expected", Data.Json.String expected);
    ("actual", Data.Json.String actual);
  ]

let unsupported_reason_to_json = fun value ->
  match value with
  | LiteralOutsideSupportedSubset { supported_literals } -> Data.Json.Object [
    ("tag", Data.Json.String "literal_outside_supported_subset");
    ("supported_literals", Data.Json.Array (List.map supported_literal_to_json supported_literals));
  ]

let cst_builder_error_to_json = fun (error: Syn.CstBuilder.error) ->
  Data.Json.Object [
    ("message", Data.Json.String error.message);
    ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string error.syntax_kind));
    ("span", span_to_json error.span);
    ("context", Data.Json.Array (List.map (fun value -> Data.Json.String value) error.context));
  ]

let fields_to_json = fun value ->
  match value with
  | CstBuilderError { builder_error } ->
      [ ("builder_error", cst_builder_error_to_json builder_error); ]
  | UnsupportedSyntax {
    syntax_span;
    syntax_kind;
    context;
    recovery;
    reason
  } ->
      let reason_json =
        match reason with
        | Some reason -> unsupported_reason_to_json reason
        | None -> Data.Json.Null
      in
      [
        ("syntax_span", span_to_json syntax_span);
        ("syntax_kind", Data.Json.String (Syn.SyntaxKind.to_string syntax_kind));
        ("context", Data.Json.String (unsupported_context_tag context));
        ("recovery", Data.Json.String (unsupported_recovery_tag recovery));
        ("reason", reason_json);
      ]
  | IgnoredPatternTypeConstraint { constraint_span } ->
      [ ("constraint_span", span_to_json constraint_span); ]
  | ParameterLoweredAsPositional { parameter_span } ->
      [ ("parameter_span", span_to_json parameter_span); ]
  | ApplicationArgumentLoweredAsPositional { application_span } ->
      [ ("application_span", span_to_json application_span); ]
  | IgnoredTypeAscription { ascription_span } ->
      [ ("ascription_span", span_to_json ascription_span); ]
  | IgnoredPolymorphicAnnotation { annotation_span } ->
      [ ("annotation_span", span_to_json annotation_span); ]
  | UnsupportedInterfaceFile { interface_span } ->
      [ ("interface_span", span_to_json interface_span); ]
  | UnboundName { reference_span; name } ->
      [ ("reference_span", span_to_json reference_span); ("name_text", Data.Json.String name); ]
  | TypeMismatch { mismatch_span; mismatch } ->
      [ ("mismatch_span", span_to_json mismatch_span); ("mismatch", mismatch_to_json mismatch); ]
  | ApplicationLabelMismatch { application_span; expected_label; actual_labels } ->
      [
        ("application_span", span_to_json application_span);
        ("expected_label", application_label_to_json expected_label);
        ("actual_labels", Data.Json.Array (List.map application_label_to_json actual_labels));
      ]
  | RecordResolutionError { operation_span; context; reason } ->
      [
        ("operation_span", span_to_json operation_span);
        ("context", record_context_to_json context);
        ("reason", record_resolution_reason_to_json reason);
      ]
  | OrPatternBindingsMismatch { pattern_span; expected_names; actual_names } ->
      [
        ("pattern_span", span_to_json pattern_span);
        ("expected_names", names_to_json expected_names);
        ("actual_names", names_to_json actual_names);
      ]
  | NonexhaustiveMatch { match_span; witness } ->
      [ ("match_span", span_to_json match_span); ("witness", match_witness_to_json witness); ]
  | RedundantMatchCase { case_span } ->
      [ ("case_span", span_to_json case_span); ]
  | SignatureInclusionError { mismatch_span; counterpart_span; mismatch } ->
      let counterpart_json =
        match counterpart_span with
        | Some span -> span_to_json span
        | None -> Data.Json.Null
      in
      [
        ("mismatch_span", span_to_json mismatch_span);
        ("counterpart_span", counterpart_json);
        ("mismatch", signature_mismatch_to_json mismatch);
        ("subject", Data.Json.String (signature_mismatch_name mismatch));
      ]
  | UnsupportedSemanticExpression { expression_span; summary } ->
      [ ("expression_span", span_to_json expression_span); ("summary", Data.Json.String summary); ]
  | RecursiveGroupRequiresSimpleVariableBinders { binding_span } ->
      [ ("binding_span", span_to_json binding_span); ]

let to_json = fun diagnostic ->
  Data.Json.Object ([
    ("id", Data.Json.String (code diagnostic));
    ("name", Data.Json.String (name diagnostic));
    ("message", Data.Json.String (message diagnostic));
    ("severity", Data.Json.String (severity_to_string (severity diagnostic)));
  ]
  @ fields_to_json diagnostic)

let to_string = fun diagnostic ->
  format
    Format.[
      str (severity_to_string (severity diagnostic));
      str " ";
      str (code diagnostic);
      str " @ ";
      str (Syn.Ceibo.Span.to_string (primary_span diagnostic));
      str ": ";
      str (message diagnostic);
    ]
