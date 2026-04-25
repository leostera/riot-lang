open Std
open Std.Collections

type t = Semantic_tree.t

let lower = Lower.lower_source_file

let span_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "start" Serde.Ser.int
            (
              fun (span: Syn.Ceibo.Span.t) -> span.start
            );
          Serde.Ser.field "end" Serde.Ser.int
            (
              fun (span: Syn.Ceibo.Span.t) -> span.end_
            );
        ]
    )

let file_kind_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.unit "Implementation"
        (
          function
          | `Implementation -> true
          | `Interface -> false
        );
      Serde.Ser.Variant.unit "Interface"
        (
          function
          | `Implementation -> false
          | `Interface -> true
        );
    ]

let path_serializer = Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string)

let binding_id_serializer = Model.Binding_id.serializer

let arrow_label_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "name" Serde.Ser.string
            (
              fun (value: Semantic_tree.arrow_label) -> value.name
            );
          Serde.Ser.field "optional" Serde.Ser.bool
            (
              fun (value: Semantic_tree.arrow_label) -> value.optional_
            );
        ]
    )

let rec type_expr_serializer = {
  Serde.Ser.run = fun backend state value ->
    let type_constr_serializer =
      Serde.Ser.record
        (
          Serde.Ser.fields
            [
              Serde.Ser.field "path" path_serializer
                (
                  fun (item: Semantic_tree.type_constr) -> item.path
                );
              Serde.Ser.field "arguments" (Serde.Ser.contramap Array.from_list (Serde.Ser.array type_expr_serializer))
                (
                  fun (item: Semantic_tree.type_constr) -> item.arguments
                );
            ]
        )
    in
    let type_alias_serializer =
      Serde.Ser.record
        (
          Serde.Ser.fields
            [
              Serde.Ser.field "type" type_expr_serializer
                (
                  fun (item: Semantic_tree.type_alias) -> item.type_
                );
              Serde.Ser.field "name" Serde.Ser.string
                (
                  fun (item: Semantic_tree.type_alias) -> item.name
                );
            ]
        )
    in
    let type_poly_serializer =
      Serde.Ser.record
        (
          Serde.Ser.fields
            [
              Serde.Ser.field "binders" (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string))
                (
                  fun (item: Semantic_tree.type_poly) -> item.binders
                );
              Serde.Ser.field "body" type_expr_serializer
                (
                  fun (item: Semantic_tree.type_poly) -> item.body
                );
            ]
        )
    in
    let type_arrow_serializer =
      Serde.Ser.record
        (
          Serde.Ser.fields
            [
              Serde.Ser.field "label" (Serde.Ser.option arrow_label_serializer)
                (
                  fun (item: Semantic_tree.type_arrow) -> item.label
                );
              Serde.Ser.field "parameter" type_expr_serializer
                (
                  fun (item: Semantic_tree.type_arrow) -> item.parameter
                );
              Serde.Ser.field "result" type_expr_serializer
                (
                  fun (item: Semantic_tree.type_arrow) -> item.result
                );
            ]
        )
    in
    let serializer =
      Serde.Ser.variant
        [
          Serde.Ser.Variant.unit "AnyType"
            (
              function
              | Semantic_tree.AnyType -> true
              | _ -> false
            );
          Serde.Ser.Variant.newtype "TypeVar" Serde.Ser.string
            (
              function
              | Semantic_tree.TypeVar name -> Some name
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypeConstr" type_constr_serializer
            (
              function
              | Semantic_tree.TypeConstr item -> Some item
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypeAlias" type_alias_serializer
            (
              function
              | Semantic_tree.TypeAlias item -> Some item
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypePoly" type_poly_serializer
            (
              function
              | Semantic_tree.TypePoly item -> Some item
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypeArrow" type_arrow_serializer
            (
              function
              | Semantic_tree.TypeArrow item -> Some item
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypeTuple" (Serde.Ser.contramap Array.from_list (Serde.Ser.array type_expr_serializer))
            (
              function
              | Semantic_tree.TypeTuple items -> Some items
              | _ -> None
            );
          Serde.Ser.Variant.newtype "TypeUnsupported" Serde.Ser.string
            (
              function
              | Semantic_tree.TypeUnsupported summary -> Some summary
              | _ -> None
            );
        ]
    in
    serializer.run backend state value
}

let module_definition_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "Alias" path_serializer
        (
          function
          | Semantic_tree.Alias path -> Some path
          | Semantic_tree.Opaque -> None
        );
      Serde.Ser.Variant.unit "Opaque"
        (
          function
          | Semantic_tree.Alias _ -> false
          | Semantic_tree.Opaque -> true
        );
    ]

let include_target_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "ModulePath" path_serializer
        (
          function
          | Semantic_tree.ModulePath path -> Some path
          | Semantic_tree.ModuleTypePath _ | Semantic_tree.Opaque -> None
        );
      Serde.Ser.Variant.newtype "ModuleTypePath" path_serializer
        (
          function
          | Semantic_tree.ModuleTypePath path -> Some path
          | Semantic_tree.ModulePath _ | Semantic_tree.Opaque -> None
        );
      Serde.Ser.Variant.unit "Opaque"
        (
          function
          | Semantic_tree.Opaque -> true
          | Semantic_tree.ModulePath _ | Semantic_tree.ModuleTypePath _ -> false
        );
    ]

let exception_rhs_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "ExceptionAlias" path_serializer
        (
          function
          | Semantic_tree.ExceptionAlias path -> Some path
          | Semantic_tree.ExceptionPayload _ -> None
        );
      Serde.Ser.Variant.newtype "ExceptionPayload" type_expr_serializer
        (
          function
          | Semantic_tree.ExceptionPayload type_ -> Some type_
          | Semantic_tree.ExceptionAlias _ -> None
        );
    ]

let type_declaration_serializer = let open Serde.Ser in
record
  (
    fields
      [
        field "id" binding_id_serializer
          (
            fun (value: Semantic_tree.type_declaration) -> value.id
          );
        field "span" span_serializer
          (
            fun (value: Semantic_tree.type_declaration) -> value.span
          );
        field "name" string
          (
            fun (value: Semantic_tree.type_declaration) -> value.name
          );
        field "params" (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string))
          (
            fun (value: Semantic_tree.type_declaration) -> value.params
          );
        field "manifest" (Serde.Ser.option type_expr_serializer)
          (
            fun (value: Semantic_tree.type_declaration) -> value.manifest
          );
        field "nonrec" bool
          (
            fun (value: Semantic_tree.type_declaration) -> value.nonrec_
          );
        field "private" bool
          (
            fun (value: Semantic_tree.type_declaration) -> value.private_
          );
      ]
  )

let value_declaration_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "id" binding_id_serializer
            (
              fun (value: Semantic_tree.value_declaration) -> value.id
            );
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.value_declaration) -> value.span
            );
          Serde.Ser.field "name" (Serde.Ser.option Serde.Ser.string)
            (
              fun (value: Semantic_tree.value_declaration) -> value.name
            );
          Serde.Ser.field "recursive" Serde.Ser.bool
            (
              fun (value: Semantic_tree.value_declaration) -> value.recursive
            );
          Serde.Ser.field "parameter_count" Serde.Ser.int
            (
              fun (value: Semantic_tree.value_declaration) -> value.parameter_count
            );
          Serde.Ser.field "declared" Serde.Ser.bool
            (
              fun (value: Semantic_tree.value_declaration) -> value.declared
            );
          Serde.Ser.field "annotation" (Serde.Ser.option type_expr_serializer)
            (
              fun (value: Semantic_tree.value_declaration) -> value.annotation
            );
        ]
    )

let module_declaration_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "id" binding_id_serializer
            (
              fun (value: Semantic_tree.module_declaration) -> value.id
            );
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.module_declaration) -> value.span
            );
          Serde.Ser.field "name" Serde.Ser.string
            (
              fun (value: Semantic_tree.module_declaration) -> value.name
            );
          Serde.Ser.field "recursive" Serde.Ser.bool
            (
              fun (value: Semantic_tree.module_declaration) -> value.recursive
            );
          Serde.Ser.field "definition" module_definition_serializer
            (
              fun (value: Semantic_tree.module_declaration) -> value.definition
            );
        ]
    )

let module_type_declaration_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "id" binding_id_serializer
            (
              fun (value: Semantic_tree.module_type_declaration) -> value.id
            );
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.module_type_declaration) -> value.span
            );
          Serde.Ser.field "name" Serde.Ser.string
            (
              fun (value: Semantic_tree.module_type_declaration) -> value.name
            );
          Serde.Ser.field "has_definition" Serde.Ser.bool
            (
              fun (value: Semantic_tree.module_type_declaration) -> value.has_definition
            );
        ]
    )

let open_statement_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.open_statement) -> value.span
            );
          Serde.Ser.field "target" (Serde.Ser.option path_serializer)
            (
              fun (value: Semantic_tree.open_statement) -> value.target
            );
          Serde.Ser.field "override" Serde.Ser.bool
            (
              fun (value: Semantic_tree.open_statement) -> value.override_
            );
        ]
    )

let include_statement_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.include_statement) -> value.span
            );
          Serde.Ser.field "target" include_target_serializer
            (
              fun (value: Semantic_tree.include_statement) -> value.target
            );
        ]
    )

let exception_declaration_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "id" binding_id_serializer
            (
              fun (value: Semantic_tree.exception_declaration) -> value.id
            );
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.exception_declaration) -> value.span
            );
          Serde.Ser.field "name" Serde.Ser.string
            (
              fun (value: Semantic_tree.exception_declaration) -> value.name
            );
          Serde.Ser.field "rhs" (Serde.Ser.option exception_rhs_serializer)
            (
              fun (value: Semantic_tree.exception_declaration) -> value.rhs
            );
        ]
    )

let external_declaration_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "id" binding_id_serializer
            (
              fun (value: Semantic_tree.external_declaration) -> value.id
            );
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.external_declaration) -> value.span
            );
          Serde.Ser.field "name" Serde.Ser.string
            (
              fun (value: Semantic_tree.external_declaration) -> value.name
            );
          Serde.Ser.field "annotation" type_expr_serializer
            (
              fun (value: Semantic_tree.external_declaration) -> value.annotation
            );
        ]
    )

let expression_item_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.expression_item) -> value.span
            );
        ]
    )

let unsupported_item_serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "span" span_serializer
            (
              fun (value: Semantic_tree.unsupported_item) -> value.span
            );
          Serde.Ser.field "kind" (Serde.Ser.contramap Syn.SyntaxKind.to_string Serde.Ser.string)
            (
              fun (value: Semantic_tree.unsupported_item) -> value.kind
            );
          Serde.Ser.field "summary" Serde.Ser.string
            (
              fun (value: Semantic_tree.unsupported_item) -> value.summary
            );
        ]
    )

let item_serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "TypeDeclaration" type_declaration_serializer
        (
          function
          | Semantic_tree.TypeDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "ValueDeclaration" value_declaration_serializer
        (
          function
          | Semantic_tree.ValueDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "ModuleDeclaration" module_declaration_serializer
        (
          function
          | Semantic_tree.ModuleDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "ModuleTypeDeclaration" module_type_declaration_serializer
        (
          function
          | Semantic_tree.ModuleTypeDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "OpenStatement" open_statement_serializer
        (
          function
          | Semantic_tree.OpenStatement value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "IncludeStatement" include_statement_serializer
        (
          function
          | Semantic_tree.IncludeStatement value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "ExceptionDeclaration" exception_declaration_serializer
        (
          function
          | Semantic_tree.ExceptionDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "ExternalDeclaration" external_declaration_serializer
        (
          function
          | Semantic_tree.ExternalDeclaration value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "Expression" expression_item_serializer
        (
          function
          | Semantic_tree.Expression value -> Some value
          | _ -> None
        );
      Serde.Ser.Variant.newtype "Unsupported" unsupported_item_serializer
        (
          function
          | Semantic_tree.Unsupported value -> Some value
          | _ -> None
        );
    ]

let diagnostics_serializer = Serde.Ser.contramap Array.from_list (Serde.Ser.array Diagnostics.Diagnostic.serializer)

let serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "kind" file_kind_serializer
            (
              fun (value: t) -> value.kind
            );
          Serde.Ser.field "items" (Serde.Ser.contramap Array.from_list (Serde.Ser.array item_serializer))
            (
              fun (value: t) -> value.items
            );
          Serde.Ser.field "exports" (Serde.Ser.contramap Array.from_list (Serde.Ser.array item_serializer))
            (
              fun (value: t) -> value.exports
            );
          Serde.Ser.field "diagnostics" diagnostics_serializer
            (
              fun (value: t) -> value.diagnostics
            );
        ]
    )
