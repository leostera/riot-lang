open Std

(**
   Ast-driven syntax visitor.

   A visitor bundles caller context, hook callbacks, and an internal arena used
   to memoize typed Ast views while walking a syntax tree. Callers thread the
   returned visitor state through traversal instead of passing arena/cache
   values around explicitly.
*)
type action =
  | Continue
  | Skip_subtree
type 'ctx t
type 'ctx enter_node = 'ctx t -> Ast.Node.t -> 'ctx t * action
type 'ctx leave_node = 'ctx t -> Ast.Node.t -> 'ctx t
type 'ctx enter_token = 'ctx t -> Ast.Token.t -> 'ctx t
type 'ctx enter_structure_item = 'ctx t -> Ast.StructureItem.t -> 'ctx t * action
type 'ctx enter_signature_item = 'ctx t -> Ast.SignatureItem.t -> 'ctx t * action
type 'ctx enter_let_declaration = 'ctx t -> Ast.LetDeclaration.t -> 'ctx t * action
type 'ctx enter_let_binding = 'ctx t -> Ast.LetBinding.t -> 'ctx t * action
type 'ctx enter_type_declaration = 'ctx t -> Ast.TypeDeclaration.t -> 'ctx t * action
type 'ctx enter_module_declaration = 'ctx t -> Ast.ModuleDeclaration.t -> 'ctx t * action
type 'ctx enter_module_functor_parameter =
  'ctx t ->
  Ast.ModuleDeclaration.Member.functor_parameter ->
  'ctx t * action
type 'ctx enter_module_type_declaration = 'ctx t -> Ast.ModuleTypeDeclaration.t -> 'ctx t * action
type 'ctx enter_open_declaration = 'ctx t -> Ast.OpenDeclaration.t -> 'ctx t * action
type 'ctx enter_include_declaration = 'ctx t -> Ast.IncludeDeclaration.t -> 'ctx t * action
type 'ctx enter_value_declaration = 'ctx t -> Ast.ValueDeclaration.t -> 'ctx t * action
type 'ctx enter_expr = 'ctx t -> Ast.Expr.t -> 'ctx t * action
type 'ctx enter_pattern = 'ctx t -> Ast.Pattern.t -> 'ctx t * action
type 'ctx enter_parameter = 'ctx t -> Ast.Parameter.t -> 'ctx t * action
type 'ctx enter_type_expr = 'ctx t -> Ast.TypeExpr.t -> 'ctx t * action
type 'ctx hooks = {
  enter_node: 'ctx enter_node option;
  leave_node: 'ctx leave_node option;
  enter_token: 'ctx enter_token option;
  enter_structure_item: 'ctx enter_structure_item option;
  enter_signature_item: 'ctx enter_signature_item option;
  enter_let_declaration: 'ctx enter_let_declaration option;
  enter_let_binding: 'ctx enter_let_binding option;
  enter_type_declaration: 'ctx enter_type_declaration option;
  enter_module_declaration: 'ctx enter_module_declaration option;
  enter_module_functor_parameter: 'ctx enter_module_functor_parameter option;
  enter_module_type_declaration: 'ctx enter_module_type_declaration option;
  enter_open_declaration: 'ctx enter_open_declaration option;
  enter_include_declaration: 'ctx enter_include_declaration option;
  enter_value_declaration: 'ctx enter_value_declaration option;
  enter_expr: 'ctx enter_expr option;
  enter_pattern: 'ctx enter_pattern option;
  enter_parameter: 'ctx enter_parameter option;
  enter_type_expr: 'ctx enter_type_expr option;
}

val empty_hooks: 'ctx hooks

val make: ctx:'ctx -> hooks:'ctx hooks -> 'ctx t

val ctx: 'ctx t -> 'ctx

val with_ctx: 'ctx t -> 'ctx -> 'ctx t

val visit_source_file: 'ctx t -> Ast.SourceFile.t -> 'ctx t

(**
   Raw-node traversal escape hatch for syntax utilities. Prefer typed
   entrypoints below when the caller already has a typed Ast handle.
*)
val visit_node: 'ctx t -> Ast.Node.t -> 'ctx t

val visit_implementation: 'ctx t -> Ast.Implementation.t -> 'ctx t

val visit_interface: 'ctx t -> Ast.Interface.t -> 'ctx t

val visit_structure_item: 'ctx t -> Ast.StructureItem.t -> 'ctx t

val visit_signature_item: 'ctx t -> Ast.SignatureItem.t -> 'ctx t

val visit_let_declaration: 'ctx t -> Ast.LetDeclaration.t -> 'ctx t

val visit_let_binding: 'ctx t -> Ast.LetBinding.t -> 'ctx t

val visit_type_declaration: 'ctx t -> Ast.TypeDeclaration.t -> 'ctx t

val visit_type_extension_declaration: 'ctx t -> Ast.TypeExtensionDeclaration.t -> 'ctx t

val visit_module_declaration: 'ctx t -> Ast.ModuleDeclaration.t -> 'ctx t

val visit_module_expr: 'ctx t -> Ast.ModuleExpr.t -> 'ctx t

val visit_module_type_expr: 'ctx t -> Ast.ModuleTypeExpr.t -> 'ctx t

val visit_module_type_declaration: 'ctx t -> Ast.ModuleTypeDeclaration.t -> 'ctx t

val visit_module_type_constraint: 'ctx t -> Ast.ModuleTypeConstraint.t -> 'ctx t

val visit_open_declaration: 'ctx t -> Ast.OpenDeclaration.t -> 'ctx t

val visit_include_declaration: 'ctx t -> Ast.IncludeDeclaration.t -> 'ctx t

val visit_value_declaration: 'ctx t -> Ast.ValueDeclaration.t -> 'ctx t

val visit_external_declaration: 'ctx t -> Ast.ExternalDeclaration.t -> 'ctx t

val visit_exception_declaration: 'ctx t -> Ast.ExceptionDeclaration.t -> 'ctx t

val visit_extension_item: 'ctx t -> Ast.ExtensionItem.t -> 'ctx t

val visit_attribute_item: 'ctx t -> Ast.AttributeItem.t -> 'ctx t

val visit_expr_item: 'ctx t -> Ast.ExprItem.t -> 'ctx t

val visit_expr: 'ctx t -> Ast.Expr.t -> 'ctx t

val visit_pattern: 'ctx t -> Ast.Pattern.t -> 'ctx t

val visit_parameter: 'ctx t -> Ast.Parameter.t -> 'ctx t

val visit_match_case: 'ctx t -> Ast.MatchCase.t -> 'ctx t

val visit_type_expr: 'ctx t -> Ast.TypeExpr.t -> 'ctx t

val visit_record_type: 'ctx t -> Ast.RecordType.t -> 'ctx t

val visit_record_field: 'ctx t -> Ast.RecordField.t -> 'ctx t

val visit_record_expr_field: 'ctx t -> Ast.RecordExprField.t -> 'ctx t

val visit_variant_type: 'ctx t -> Ast.VariantType.t -> 'ctx t

val visit_variant_constructor: 'ctx t -> Ast.VariantConstructor.t -> 'ctx t

val visit_ident: 'ctx t -> Ast.Ident.t -> 'ctx t

val structure_item: 'ctx t -> Ast.Node.t -> Ast.StructureItem.t option

val signature_item: 'ctx t -> Ast.Node.t -> Ast.SignatureItem.t option

val let_declaration: 'ctx t -> Ast.Node.t -> Ast.LetDeclaration.t option

val let_binding: 'ctx t -> Ast.Node.t -> Ast.LetBinding.t option

val type_declaration: 'ctx t -> Ast.Node.t -> Ast.TypeDeclaration.t option

val module_declaration: 'ctx t -> Ast.Node.t -> Ast.ModuleDeclaration.t option

val module_type_declaration: 'ctx t -> Ast.Node.t -> Ast.ModuleTypeDeclaration.t option

val open_declaration: 'ctx t -> Ast.Node.t -> Ast.OpenDeclaration.t option

val include_declaration: 'ctx t -> Ast.Node.t -> Ast.IncludeDeclaration.t option

val value_declaration: 'ctx t -> Ast.Node.t -> Ast.ValueDeclaration.t option

val expr: 'ctx t -> Ast.Node.t -> Ast.Expr.t option

val pattern: 'ctx t -> Ast.Node.t -> Ast.Pattern.t option

val parameter: 'ctx t -> Ast.Node.t -> Ast.Parameter.t option

val type_expr: 'ctx t -> Ast.Node.t -> Ast.TypeExpr.t option
