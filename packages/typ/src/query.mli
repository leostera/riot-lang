(**
   Query helpers over a checked `Typ.Ast`.

   `Typ.Query` is the editor/query layer for the one-shot checker. A context
   keeps the typed source file together with the inference result for that
   file, so callers such as `riot-lsp` can answer many semantic questions
   without rechecking the file for each request.
*)

(** Query context for one checked source snapshot. *)
type context

(** Parent-linked typed node returned by tree queries. *)
module Node: sig
  (** Typed node kind. *)
  type kind =
    | SourceFile of Ast.t
    | StructureItem of Ast.structure_item
    | SignatureItem of Ast.signature_item
    | LetDeclaration of Ast.let_declaration
    | LetBinding of Ast.let_binding
    | ValueDeclaration of Ast.value_declaration
    | ExternalDeclaration of Ast.external_declaration
    | TypeDeclaration of Ast.type_declaration
    | TypeDefinition of Ast.type_definition
    | TypeConstructor of Ast.type_constructor
    | TypeExtensionDeclaration of Ast.type_extension_declaration
    | RecordFieldDeclaration of Ast.record_field_declaration
    | RecordExpressionField of Ast.record_expression_field
    | RecordPatternField of Ast.record_pattern_field
    | ExceptionDeclaration of Ast.exception_declaration
    | ModuleDeclaration of Ast.module_declaration
    | ModuleTypeDeclaration of Ast.module_type_declaration
    | FunctorParameter of Ast.functor_parameter
    | ModuleUnpack of Ast.module_unpack
    | PackageType of Ast.package_type
    | PackageTypeConstraint of Ast.package_type_constraint
    | Parameter of Ast.parameter
    | Argument of Ast.argument
    | MatchCase of Ast.match_case
    | Pattern of Ast.pattern
    | Expression of Ast.expression
    | CoreType of Ast.core_type
    | PolyVariantTypeField of Ast.poly_variant_type_field
  type t = {
    kind: kind;
    parent: t option;
  }

  (** Return the typed kind carried by a query node. *)
  val kind: t -> kind

  (** Return the parent query node, if any. *)
  val parent: t -> t option

  (** Return the source span for a node. *)
  val span: t -> Syn.Span.t
end

(**
   Create a query context for a checked source snapshot.

   `source_file` must be the same tree passed to `Infer.check` to produce
   `infer_result`.
*)
val create: source_file:Ast.t -> infer_result:Infer.infer_result -> context

(** Return the typed source file stored in the context. *)
val source_file: context -> Ast.t

(** Return the inference result stored in the context. *)
val infer_result: context -> Infer.infer_result

(**
   Return the smallest typed node containing `span`, if one exists.

   A zero-width span is treated as a cursor position and may match a node end
   boundary. Non-empty spans must be fully contained by the returned node.
*)
val node_at: context -> Syn.Span.t -> Node.t option

(**
   Return the root-to-leaf node path containing `span`.

   The final element is the same node returned by `node_at`.
*)
val path_at: context -> Syn.Span.t -> Node.t list
