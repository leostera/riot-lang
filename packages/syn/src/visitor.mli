open Std

(** Ast-driven syntax visitor.

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

val visit_node: 'ctx t -> Ast.Node.t -> 'ctx t

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
