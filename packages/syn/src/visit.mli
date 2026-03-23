open Std

(** Explicit visitor-style traversal over the typed CST.

    {!Visit} is the shared recursion API on top of {!Cst}. Its purpose is to
    let syntax consumers write one callback per node family they care about and
    keep traversal control explicit.

    This module is intentionally different from a fold:

    - callbacks do **not** return `Continue` / `Skip` / `Stop`
    - callbacks do **not** trigger any implicit child walk
    - callbacks return an updated `'ctx`
    - callbacks decide whether to recurse, which children to recurse into, and
      in what order

    The central distinction is:

    - the **visitor** is the hook table
    - the **walker** is the traversal engine

    A callback receives both the current `'ctx` and the current `'ctx walker`.
    It can then:

    - inspect the current node
    - update the context
    - recurse into child nodes by calling `walker.<node_family>`
    - reuse the standard child traversal for the current node by calling
      `walker.descend_<node_family>`

    This means traversal control stays entirely explicit.

    Example: collect all `match` expressions.

    ```ocaml
    let visitor =
      {
        Syn.Visit.default with
        visit_expression =
          (fun expressions walk expr ->
            let expressions =
              match expr with
              | Syn.Cst.Expression.Match _ -> expr :: expressions
              | _ -> expressions
            in
            walk.descend_expression expressions expr);
      }
    in
    Syn.Visit.source_file visitor [] source_file |> List.rev
    ```

    Example: inspect `let` bindings but skip their bodies entirely.

    ```ocaml
    let visitor =
      {
        Syn.Visit.default with
        visit_let_binding =
          (fun names _walk binding ->
            match Syn.Cst.LetBinding.binding_name_token binding with
            | Some token -> Syn.Cst.Token.text token :: names
            | None -> names);
      }
    in
    Syn.Visit.source_file visitor [] source_file |> List.rev
    ```

    Example: traverse an `if` expression in a custom order.

    ```ocaml
    let visitor =
      {
        Syn.Visit.default with
        visit_expression =
          (fun ctx walk expr ->
            match expr with
            | Syn.Cst.Expression.If if_expr ->
                let ctx = walk.expression ctx if_expr.then_branch in
                let ctx =
                  match if_expr.else_branch with
                  | Some else_branch -> walk.expression ctx else_branch
                  | None -> ctx
                in
                walk.expression ctx if_expr.condition
            | _ ->
                walk.descend_expression ctx expr);
      }
    ```

    This module is meant to be the one shared traversal layer for `syn`. Small
    syntactic helpers belong in `Syn.Matchers`; faithful syntax representation
    belongs in `Syn.Cst`; explicit recursion belongs here.
*)

type 'ctx walker = {
  apply_argument : 'ctx -> Cst.apply_argument -> 'ctx;
  attribute : 'ctx -> Cst.attribute -> 'ctx;
  binding_operator_binding :
    'ctx -> Cst.binding_operator_binding -> 'ctx;
  class_declaration : 'ctx -> Cst.class_declaration -> 'ctx;
  class_expression : 'ctx -> Cst.ClassExpression.t -> 'ctx;
  class_field : 'ctx -> Cst.class_field -> 'ctx;
  class_type : 'ctx -> Cst.ClassType.t -> 'ctx;
  class_type_declaration : 'ctx -> Cst.class_type_declaration -> 'ctx;
  class_type_field : 'ctx -> Cst.ClassTypeField.t -> 'ctx;
  core_type : 'ctx -> Cst.CoreType.t -> 'ctx;
  exception_declaration : 'ctx -> Cst.exception_declaration -> 'ctx;
  expression : 'ctx -> Cst.Expression.t -> 'ctx;
  extension : 'ctx -> Cst.extension -> 'ctx;
  external_declaration : 'ctx -> Cst.external_declaration -> 'ctx;
  functor_parameter : 'ctx -> Cst.FunctorParameter.t -> 'ctx;
  implementation : 'ctx -> Cst.implementation -> 'ctx;
  include_statement : 'ctx -> Cst.include_statement -> 'ctx;
  interface : 'ctx -> Cst.interface -> 'ctx;
  let_binding : 'ctx -> Cst.LetBinding.t -> 'ctx;
  match_case : 'ctx -> Cst.match_case -> 'ctx;
  module_declaration : 'ctx -> Cst.ModuleDeclaration.t -> 'ctx;
  module_expression : 'ctx -> Cst.ModuleExpression.t -> 'ctx;
  module_type : 'ctx -> Cst.ModuleType.t -> 'ctx;
  module_type_constraint :
    'ctx -> Cst.ModuleTypeConstraint.t -> 'ctx;
  module_type_declaration :
    'ctx -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  object_member : 'ctx -> Cst.ObjectMember.t -> 'ctx;
  object_type_field : 'ctx -> Cst.object_type_field -> 'ctx;
  open_statement : 'ctx -> Cst.OpenStatement.t -> 'ctx;
  parameter : 'ctx -> Cst.Parameter.t -> 'ctx;
  pattern : 'ctx -> Cst.Pattern.t -> 'ctx;
  pattern_payload : 'ctx -> Cst.PatternPayload.t -> 'ctx;
  payload : 'ctx -> Cst.Payload.t -> 'ctx;
  record_expression : 'ctx -> Cst.RecordExpression.t -> 'ctx;
  record_type_field : 'ctx -> Cst.record_type_field -> 'ctx;
  recursive_module_declaration :
    'ctx -> Cst.RecursiveModuleDeclaration.t -> 'ctx;
  row_field : 'ctx -> Cst.RowField.t -> 'ctx;
  signature_item : 'ctx -> Cst.SignatureItem.t -> 'ctx;
  source_file : 'ctx -> Cst.SourceFile.t -> 'ctx;
  structure_item : 'ctx -> Cst.StructureItem.t -> 'ctx;
  type_binder : 'ctx -> Cst.TypeBinder.t -> 'ctx;
  type_constraint : 'ctx -> Cst.TypeConstraint.t -> 'ctx;
  type_declaration : 'ctx -> Cst.TypeDeclaration.t -> 'ctx;
  type_definition : 'ctx -> Cst.TypeDefinition.t -> 'ctx;
  type_extension : 'ctx -> Cst.TypeExtension.t -> 'ctx;
  type_parameter : 'ctx -> Cst.TypeParameter.t -> 'ctx;
  value_declaration : 'ctx -> Cst.value_declaration -> 'ctx;
  variant_constructor : 'ctx -> Cst.VariantConstructor.t -> 'ctx;
  descend_apply_argument : 'ctx -> Cst.apply_argument -> 'ctx;
  descend_attribute : 'ctx -> Cst.attribute -> 'ctx;
  descend_binding_operator_binding :
    'ctx -> Cst.binding_operator_binding -> 'ctx;
  descend_class_declaration : 'ctx -> Cst.class_declaration -> 'ctx;
  descend_class_expression : 'ctx -> Cst.ClassExpression.t -> 'ctx;
  descend_class_field : 'ctx -> Cst.class_field -> 'ctx;
  descend_class_type : 'ctx -> Cst.ClassType.t -> 'ctx;
  descend_class_type_declaration :
    'ctx -> Cst.class_type_declaration -> 'ctx;
  descend_class_type_field :
    'ctx -> Cst.ClassTypeField.t -> 'ctx;
  descend_core_type : 'ctx -> Cst.CoreType.t -> 'ctx;
  descend_exception_declaration :
    'ctx -> Cst.exception_declaration -> 'ctx;
  descend_expression : 'ctx -> Cst.Expression.t -> 'ctx;
  descend_extension : 'ctx -> Cst.extension -> 'ctx;
  descend_external_declaration :
    'ctx -> Cst.external_declaration -> 'ctx;
  descend_functor_parameter :
    'ctx -> Cst.FunctorParameter.t -> 'ctx;
  descend_implementation : 'ctx -> Cst.implementation -> 'ctx;
  descend_include_statement : 'ctx -> Cst.include_statement -> 'ctx;
  descend_interface : 'ctx -> Cst.interface -> 'ctx;
  descend_let_binding : 'ctx -> Cst.LetBinding.t -> 'ctx;
  descend_match_case : 'ctx -> Cst.match_case -> 'ctx;
  descend_module_declaration :
    'ctx -> Cst.ModuleDeclaration.t -> 'ctx;
  descend_module_expression :
    'ctx -> Cst.ModuleExpression.t -> 'ctx;
  descend_module_type : 'ctx -> Cst.ModuleType.t -> 'ctx;
  descend_module_type_constraint :
    'ctx -> Cst.ModuleTypeConstraint.t -> 'ctx;
  descend_module_type_declaration :
    'ctx -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  descend_object_member : 'ctx -> Cst.ObjectMember.t -> 'ctx;
  descend_object_type_field :
    'ctx -> Cst.object_type_field -> 'ctx;
  descend_open_statement : 'ctx -> Cst.OpenStatement.t -> 'ctx;
  descend_parameter : 'ctx -> Cst.Parameter.t -> 'ctx;
  descend_pattern : 'ctx -> Cst.Pattern.t -> 'ctx;
  descend_pattern_payload : 'ctx -> Cst.PatternPayload.t -> 'ctx;
  descend_payload : 'ctx -> Cst.Payload.t -> 'ctx;
  descend_record_expression :
    'ctx -> Cst.RecordExpression.t -> 'ctx;
  descend_record_type_field :
    'ctx -> Cst.record_type_field -> 'ctx;
  descend_recursive_module_declaration :
    'ctx -> Cst.RecursiveModuleDeclaration.t -> 'ctx;
  descend_row_field : 'ctx -> Cst.RowField.t -> 'ctx;
  descend_signature_item : 'ctx -> Cst.SignatureItem.t -> 'ctx;
  descend_source_file : 'ctx -> Cst.SourceFile.t -> 'ctx;
  descend_structure_item : 'ctx -> Cst.StructureItem.t -> 'ctx;
  descend_type_binder : 'ctx -> Cst.TypeBinder.t -> 'ctx;
  descend_type_constraint : 'ctx -> Cst.TypeConstraint.t -> 'ctx;
  descend_type_declaration :
    'ctx -> Cst.TypeDeclaration.t -> 'ctx;
  descend_type_definition : 'ctx -> Cst.TypeDefinition.t -> 'ctx;
  descend_type_extension : 'ctx -> Cst.TypeExtension.t -> 'ctx;
  descend_type_parameter : 'ctx -> Cst.TypeParameter.t -> 'ctx;
  descend_value_declaration :
    'ctx -> Cst.value_declaration -> 'ctx;
  descend_variant_constructor :
    'ctx -> Cst.VariantConstructor.t -> 'ctx;
}
(** Traversal engine built from a visitor.

    The `walker` exposes two families of functions for every supported node
    family:

    - `walker.expression ctx expr`
      calls the matching visitor hook for `expr`
    - `walker.descend_expression ctx expr`
      skips the current hook and performs the standard child traversal for
      `expr`

    That second family is what makes default traversal reusable without
    reintroducing implicit recursion.
*)

type 'ctx visitor = {
  visit_apply_argument :
    'ctx -> 'ctx walker -> Cst.apply_argument -> 'ctx;
  visit_attribute :
    'ctx -> 'ctx walker -> Cst.attribute -> 'ctx;
  visit_binding_operator_binding :
    'ctx -> 'ctx walker -> Cst.binding_operator_binding -> 'ctx;
  visit_class_declaration :
    'ctx -> 'ctx walker -> Cst.class_declaration -> 'ctx;
  visit_class_expression :
    'ctx -> 'ctx walker -> Cst.ClassExpression.t -> 'ctx;
  visit_class_field :
    'ctx -> 'ctx walker -> Cst.class_field -> 'ctx;
  visit_class_type :
    'ctx -> 'ctx walker -> Cst.ClassType.t -> 'ctx;
  visit_class_type_declaration :
    'ctx -> 'ctx walker -> Cst.class_type_declaration -> 'ctx;
  visit_class_type_field :
    'ctx -> 'ctx walker -> Cst.ClassTypeField.t -> 'ctx;
  visit_core_type :
    'ctx -> 'ctx walker -> Cst.CoreType.t -> 'ctx;
  visit_exception_declaration :
    'ctx -> 'ctx walker -> Cst.exception_declaration -> 'ctx;
  visit_expression :
    'ctx -> 'ctx walker -> Cst.Expression.t -> 'ctx;
  visit_extension :
    'ctx -> 'ctx walker -> Cst.extension -> 'ctx;
  visit_external_declaration :
    'ctx -> 'ctx walker -> Cst.external_declaration -> 'ctx;
  visit_functor_parameter :
    'ctx -> 'ctx walker -> Cst.FunctorParameter.t -> 'ctx;
  visit_implementation :
    'ctx -> 'ctx walker -> Cst.implementation -> 'ctx;
  visit_include_statement :
    'ctx -> 'ctx walker -> Cst.include_statement -> 'ctx;
  visit_interface :
    'ctx -> 'ctx walker -> Cst.interface -> 'ctx;
  visit_let_binding :
    'ctx -> 'ctx walker -> Cst.LetBinding.t -> 'ctx;
  visit_match_case :
    'ctx -> 'ctx walker -> Cst.match_case -> 'ctx;
  visit_module_declaration :
    'ctx -> 'ctx walker -> Cst.ModuleDeclaration.t -> 'ctx;
  visit_module_expression :
    'ctx -> 'ctx walker -> Cst.ModuleExpression.t -> 'ctx;
  visit_module_type :
    'ctx -> 'ctx walker -> Cst.ModuleType.t -> 'ctx;
  visit_module_type_constraint :
    'ctx -> 'ctx walker -> Cst.ModuleTypeConstraint.t -> 'ctx;
  visit_module_type_declaration :
    'ctx -> 'ctx walker -> Cst.ModuleTypeDeclaration.t -> 'ctx;
  visit_object_member :
    'ctx -> 'ctx walker -> Cst.ObjectMember.t -> 'ctx;
  visit_object_type_field :
    'ctx -> 'ctx walker -> Cst.object_type_field -> 'ctx;
  visit_open_statement :
    'ctx -> 'ctx walker -> Cst.OpenStatement.t -> 'ctx;
  visit_parameter :
    'ctx -> 'ctx walker -> Cst.Parameter.t -> 'ctx;
  visit_pattern :
    'ctx -> 'ctx walker -> Cst.Pattern.t -> 'ctx;
  visit_pattern_payload :
    'ctx -> 'ctx walker -> Cst.PatternPayload.t -> 'ctx;
  visit_payload :
    'ctx -> 'ctx walker -> Cst.Payload.t -> 'ctx;
  visit_record_expression :
    'ctx -> 'ctx walker -> Cst.RecordExpression.t -> 'ctx;
  visit_record_type_field :
    'ctx -> 'ctx walker -> Cst.record_type_field -> 'ctx;
  visit_recursive_module_declaration :
    'ctx -> 'ctx walker -> Cst.RecursiveModuleDeclaration.t -> 'ctx;
  visit_row_field :
    'ctx -> 'ctx walker -> Cst.RowField.t -> 'ctx;
  visit_signature_item :
    'ctx -> 'ctx walker -> Cst.SignatureItem.t -> 'ctx;
  visit_source_file :
    'ctx -> 'ctx walker -> Cst.SourceFile.t -> 'ctx;
  visit_structure_item :
    'ctx -> 'ctx walker -> Cst.StructureItem.t -> 'ctx;
  visit_type_binder :
    'ctx -> 'ctx walker -> Cst.TypeBinder.t -> 'ctx;
  visit_type_constraint :
    'ctx -> 'ctx walker -> Cst.TypeConstraint.t -> 'ctx;
  visit_type_declaration :
    'ctx -> 'ctx walker -> Cst.TypeDeclaration.t -> 'ctx;
  visit_type_definition :
    'ctx -> 'ctx walker -> Cst.TypeDefinition.t -> 'ctx;
  visit_type_extension :
    'ctx -> 'ctx walker -> Cst.TypeExtension.t -> 'ctx;
  visit_type_parameter :
    'ctx -> 'ctx walker -> Cst.TypeParameter.t -> 'ctx;
  visit_value_declaration :
    'ctx -> 'ctx walker -> Cst.value_declaration -> 'ctx;
  visit_variant_constructor :
    'ctx -> 'ctx walker -> Cst.VariantConstructor.t -> 'ctx;
}
(** Hook table for CST traversal.

    Each callback runs exactly when its node family is visited. It is
    responsible for returning the next context value and deciding how much
    further traversal should happen.

    The default pattern is:

    - inspect the current node
    - update `'ctx`
    - call the matching `walker.descend_*` helper to keep the standard child
      traversal

    If a callback omits that `descend_*` call, the subtree stops there. That is
    how subtree skipping works in this model.
*)

val default : 'ctx visitor
(** Default visitor that performs the standard child traversal for every node
    family without modifying the context. *)

val walker : 'ctx visitor -> 'ctx walker
(** Build a traversal engine from a visitor. *)

val apply_argument : 'ctx visitor -> 'ctx -> Cst.apply_argument -> 'ctx
val attribute : 'ctx visitor -> 'ctx -> Cst.attribute -> 'ctx
val binding_operator_binding :
  'ctx visitor -> 'ctx -> Cst.binding_operator_binding -> 'ctx
val class_declaration : 'ctx visitor -> 'ctx -> Cst.class_declaration -> 'ctx
val class_expression : 'ctx visitor -> 'ctx -> Cst.ClassExpression.t -> 'ctx
val class_field : 'ctx visitor -> 'ctx -> Cst.class_field -> 'ctx
val class_type : 'ctx visitor -> 'ctx -> Cst.ClassType.t -> 'ctx
val class_type_declaration :
  'ctx visitor -> 'ctx -> Cst.class_type_declaration -> 'ctx
val class_type_field :
  'ctx visitor -> 'ctx -> Cst.ClassTypeField.t -> 'ctx
val core_type : 'ctx visitor -> 'ctx -> Cst.CoreType.t -> 'ctx
val exception_declaration :
  'ctx visitor -> 'ctx -> Cst.exception_declaration -> 'ctx
val expression : 'ctx visitor -> 'ctx -> Cst.Expression.t -> 'ctx
val extension : 'ctx visitor -> 'ctx -> Cst.extension -> 'ctx
val external_declaration :
  'ctx visitor -> 'ctx -> Cst.external_declaration -> 'ctx
val functor_parameter :
  'ctx visitor -> 'ctx -> Cst.FunctorParameter.t -> 'ctx
val implementation : 'ctx visitor -> 'ctx -> Cst.implementation -> 'ctx
val include_statement :
  'ctx visitor -> 'ctx -> Cst.include_statement -> 'ctx
val interface : 'ctx visitor -> 'ctx -> Cst.interface -> 'ctx
val let_binding : 'ctx visitor -> 'ctx -> Cst.LetBinding.t -> 'ctx
val match_case : 'ctx visitor -> 'ctx -> Cst.match_case -> 'ctx
val module_declaration :
  'ctx visitor -> 'ctx -> Cst.ModuleDeclaration.t -> 'ctx
val module_expression :
  'ctx visitor -> 'ctx -> Cst.ModuleExpression.t -> 'ctx
val module_type : 'ctx visitor -> 'ctx -> Cst.ModuleType.t -> 'ctx
val module_type_constraint :
  'ctx visitor -> 'ctx -> Cst.ModuleTypeConstraint.t -> 'ctx
val module_type_declaration :
  'ctx visitor -> 'ctx -> Cst.ModuleTypeDeclaration.t -> 'ctx
val object_member : 'ctx visitor -> 'ctx -> Cst.ObjectMember.t -> 'ctx
val object_type_field :
  'ctx visitor -> 'ctx -> Cst.object_type_field -> 'ctx
val open_statement : 'ctx visitor -> 'ctx -> Cst.OpenStatement.t -> 'ctx
val parameter : 'ctx visitor -> 'ctx -> Cst.Parameter.t -> 'ctx
val pattern : 'ctx visitor -> 'ctx -> Cst.Pattern.t -> 'ctx
val pattern_payload : 'ctx visitor -> 'ctx -> Cst.PatternPayload.t -> 'ctx
val payload : 'ctx visitor -> 'ctx -> Cst.Payload.t -> 'ctx
val record_expression :
  'ctx visitor -> 'ctx -> Cst.RecordExpression.t -> 'ctx
val record_type_field :
  'ctx visitor -> 'ctx -> Cst.record_type_field -> 'ctx
val recursive_module_declaration :
  'ctx visitor -> 'ctx -> Cst.RecursiveModuleDeclaration.t -> 'ctx
val row_field : 'ctx visitor -> 'ctx -> Cst.RowField.t -> 'ctx
val signature_item : 'ctx visitor -> 'ctx -> Cst.SignatureItem.t -> 'ctx
val source_file : 'ctx visitor -> 'ctx -> Cst.SourceFile.t -> 'ctx
val structure_item : 'ctx visitor -> 'ctx -> Cst.StructureItem.t -> 'ctx
val type_binder : 'ctx visitor -> 'ctx -> Cst.TypeBinder.t -> 'ctx
val type_constraint : 'ctx visitor -> 'ctx -> Cst.TypeConstraint.t -> 'ctx
val type_declaration :
  'ctx visitor -> 'ctx -> Cst.TypeDeclaration.t -> 'ctx
val type_definition : 'ctx visitor -> 'ctx -> Cst.TypeDefinition.t -> 'ctx
val type_extension : 'ctx visitor -> 'ctx -> Cst.TypeExtension.t -> 'ctx
val type_parameter : 'ctx visitor -> 'ctx -> Cst.TypeParameter.t -> 'ctx
val value_declaration :
  'ctx visitor -> 'ctx -> Cst.value_declaration -> 'ctx
val variant_constructor :
  'ctx visitor -> 'ctx -> Cst.VariantConstructor.t -> 'ctx
