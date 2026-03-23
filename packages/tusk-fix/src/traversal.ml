open Std
open Std.Collections

include Tusk_fix_api.Traversal

type binding_site = {
  syntax_node : Syn.Cst.syntax_node;
  name_token : Syn.Cst.Token.t;
  is_function : bool;
}

let direct_non_trivia_nodes node =
  let open Syn.Ceibo.Red in
  SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Node child when not (is_trivia (SyntaxNode.kind child)) ->
           Some child
       | _ ->
           None)

let is_expression_syntax_kind = function
  | Syn.SyntaxKind.IDENT_EXPR
  | Syn.SyntaxKind.MODULE_PATH
  | Syn.SyntaxKind.OPERATOR_PATTERN
  | Syn.SyntaxKind.ATTRIBUTE_EXPR
  | Syn.SyntaxKind.EXTENSION_EXPR
  | Syn.SyntaxKind.OBJECT_EXPR
  | Syn.SyntaxKind.UNIT_LITERAL
  | Syn.SyntaxKind.METHOD_CALL_EXPR
  | Syn.SyntaxKind.NEW_EXPR
  | Syn.SyntaxKind.FIELD_ACCESS_EXPR
  | Syn.SyntaxKind.ARRAY_INDEX_EXPR
  | Syn.SyntaxKind.STRING_INDEX_EXPR
  | Syn.SyntaxKind.ASSIGN_EXPR
  | Syn.SyntaxKind.STRING_LITERAL
  | Syn.SyntaxKind.INT_LITERAL
  | Syn.SyntaxKind.FLOAT_LITERAL
  | Syn.SyntaxKind.CHAR_LITERAL
  | Syn.SyntaxKind.BOOL_LITERAL
  | Syn.SyntaxKind.ASSERT_EXPR
  | Syn.SyntaxKind.LAZY_EXPR
  | Syn.SyntaxKind.WHILE_EXPR
  | Syn.SyntaxKind.FOR_EXPR
  | Syn.SyntaxKind.APPLY_EXPR
  | Syn.SyntaxKind.POLY_VARIANT_EXPR
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_EXPR
  | Syn.SyntaxKind.LET_MODULE_EXPR
  | Syn.SyntaxKind.LET_EXPR
  | Syn.SyntaxKind.LET_REC_EXPR
  | Syn.SyntaxKind.TYPED_EXPR
  | Syn.SyntaxKind.COERCE_EXPR
  | Syn.SyntaxKind.PREFIX_EXPR
  | Syn.SyntaxKind.INFIX_EXPR
  | Syn.SyntaxKind.SEQUENCE_EXPR
  | Syn.SyntaxKind.TUPLE_EXPR
  | Syn.SyntaxKind.LIST_EXPR
  | Syn.SyntaxKind.ARRAY_EXPR
  | Syn.SyntaxKind.RECORD_EXPR
  | Syn.SyntaxKind.RECORD_UPDATE_EXPR
  | Syn.SyntaxKind.UNREACHABLE_EXPR
  | Syn.SyntaxKind.OBJECT_UPDATE_EXPR
  | Syn.SyntaxKind.LOCAL_OPEN_EXPR
  | Syn.SyntaxKind.FUN_EXPR
  | Syn.SyntaxKind.FUNCTION_EXPR
  | Syn.SyntaxKind.MATCH_EXPR
  | Syn.SyntaxKind.TRY_EXPR
  | Syn.SyntaxKind.IF_EXPR
  | Syn.SyntaxKind.PAREN_EXPR ->
      true
  | _ ->
      false

let is_parameter_like_kind = function
  | Syn.SyntaxKind.IDENT_PATTERN
  | Syn.SyntaxKind.WILDCARD_PATTERN
  | Syn.SyntaxKind.LITERAL_PATTERN
  | Syn.SyntaxKind.STRING_LITERAL
  | Syn.SyntaxKind.INT_LITERAL
  | Syn.SyntaxKind.FLOAT_LITERAL
  | Syn.SyntaxKind.CHAR_LITERAL
  | Syn.SyntaxKind.BOOL_LITERAL
  | Syn.SyntaxKind.UNIT_LITERAL
  | Syn.SyntaxKind.CONSTRUCTOR_PATTERN
  | Syn.SyntaxKind.TUPLE_PATTERN
  | Syn.SyntaxKind.LIST_PATTERN
  | Syn.SyntaxKind.ARRAY_PATTERN
  | Syn.SyntaxKind.CONS_PATTERN
  | Syn.SyntaxKind.RECORD_PATTERN
  | Syn.SyntaxKind.OR_PATTERN
  | Syn.SyntaxKind.AS_PATTERN
  | Syn.SyntaxKind.RANGE_PATTERN
  | Syn.SyntaxKind.TYPED_PATTERN
  | Syn.SyntaxKind.LAZY_PATTERN
  | Syn.SyntaxKind.EXCEPTION_PATTERN
  | Syn.SyntaxKind.PAREN_PATTERN
  | Syn.SyntaxKind.POLY_VARIANT_PATTERN
  | Syn.SyntaxKind.POLY_VARIANT_TYPE_PATTERN
  | Syn.SyntaxKind.LOCAL_OPEN_PATTERN
  | Syn.SyntaxKind.OPERATOR_PATTERN
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_PATTERN
  | Syn.SyntaxKind.LABELED_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM_DEFAULT
  | Syn.SyntaxKind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      true
  | _ ->
      false

let rec binding_name_token_from_pattern = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } ->
      Some name_token
  | Syn.Cst.Pattern.Alias { name_token; _ } ->
      Some name_token
  | Syn.Cst.Pattern.Typed { pattern; _ }
  | Syn.Cst.Pattern.Parenthesized { inner = pattern; _ } ->
      binding_name_token_from_pattern pattern
  | _ ->
      None

let expression_is_function expr =
  match Syn.Ceibo.Red.SyntaxNode.kind (Syn.Cst.Expression.syntax_node expr) with
  | Syn.SyntaxKind.FUN_EXPR | Syn.SyntaxKind.FUNCTION_EXPR ->
      true
  | _ ->
      false

let has_parameter_prefix syntax_node =
  let rec go = function
    | [] ->
        false
    | node :: rest ->
        let kind = Syn.Ceibo.Red.SyntaxNode.kind node in
        if is_expression_syntax_kind kind then
          false
        else if is_parameter_like_kind kind then
          true
        else
          go rest
  in
  match direct_non_trivia_nodes syntax_node with
  | _binding_pattern :: rest ->
      go rest
  | [] ->
      false

let binding_site_of_let_binding binding =
  match Syn.Cst.LetBinding.binding_name_token binding with
  | Some name_token ->
      Some
        {
          syntax_node = Syn.Cst.LetBinding.syntax_node binding;
          name_token;
          is_function = Syn.Cst.LetBinding.is_function binding;
        }
  | None ->
      None

let binding_site_of_expression_let ~syntax_node ~binding_pattern ~bound_value =
  match binding_name_token_from_pattern binding_pattern with
  | Some name_token ->
      Some
        {
          syntax_node;
          name_token;
          is_function = has_parameter_prefix syntax_node || expression_is_function bound_value;
        }
  | None ->
      None

let rec binding_sites_of_module_expression = function
  | Syn.Cst.ModuleExpression.Path _
  | Syn.Cst.ModuleExpression.Structure _
  | Syn.Cst.ModuleExpression.Extension _ ->
      []
  | Syn.Cst.ModuleExpression.Functor { body; _ } ->
      binding_sites_of_module_expression body
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
      binding_sites_of_module_expression callee
      @ binding_sites_of_module_expression argument
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      binding_sites_of_module_expression callee
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ }
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } ->
      binding_sites_of_module_expression module_expression
  | Syn.Cst.ModuleExpression.ModuleUnpack { expression; _ } ->
      binding_sites_of_expression expression
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      binding_sites_of_module_expression inner

and binding_sites_of_object_member = function
  | Syn.Cst.ObjectMember.Method { body; _ } ->
      Option.to_list body |> List.concat_map binding_sites_of_expression
  | Syn.Cst.ObjectMember.Value { value; _ } ->
      Option.to_list value |> List.concat_map binding_sites_of_expression
  | Syn.Cst.ObjectMember.Inherit { expression; _ } ->
      binding_sites_of_expression expression
  | Syn.Cst.ObjectMember.Extension _ ->
      []
  | Syn.Cst.ObjectMember.Initializer { body; _ } ->
      Option.to_list body |> List.concat_map binding_sites_of_expression

and binding_sites_of_function_body = function
  | Syn.Cst.Expression expression ->
      binding_sites_of_expression expression
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.concat_map binding_sites_of_match_case

and binding_sites_of_expression expr =
  match expr with
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.New _ ->
      []
  | Syn.Cst.Expression.Constructor { payload; _ } ->
      Option.to_list payload |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.Object { members; _ } ->
      members |> List.concat_map binding_sites_of_object_member
  | Syn.Cst.Expression.PolyVariant { payload; _ } ->
      Option.to_list payload |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.ModulePack { module_expression; _ } ->
      binding_sites_of_module_expression module_expression
  | Syn.Cst.Expression.LetModule { module_expression; body; _ } ->
      binding_sites_of_module_expression module_expression
      @ binding_sites_of_expression body
  | Syn.Cst.Expression.LetException { body; _ } ->
      binding_sites_of_expression body
  | Syn.Cst.Expression.Assert { asserted; _ } ->
      binding_sites_of_expression asserted
  | Syn.Cst.Expression.Lazy { body; _ } ->
      binding_sites_of_expression body
  | Syn.Cst.Expression.While { condition; body; _ } ->
      binding_sites_of_expression condition @ binding_sites_of_expression body
  | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } ->
      binding_sites_of_expression start_expr
      @ binding_sites_of_expression end_expr
      @ binding_sites_of_expression body
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      binding_sites_of_expression callee
      @ binding_sites_of_apply_argument argument
  | Syn.Cst.Expression.MethodCall { receiver; _ } ->
      binding_sites_of_expression receiver
  | Syn.Cst.Expression.Prefix { operand; _ } ->
      binding_sites_of_expression operand
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      binding_sites_of_expression receiver
  | Syn.Cst.Expression.Index { collection; index; _ } ->
      binding_sites_of_expression collection @ binding_sites_of_expression index
  | Syn.Cst.Expression.ObjectOverride { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.object_override_field) ->
             Option.to_list field.value |> List.concat_map binding_sites_of_expression)
  | Syn.Cst.Expression.InstanceVariableAssign { value; _ } ->
      binding_sites_of_expression value
  | Syn.Cst.Expression.FieldAssign { target; value; _ } ->
      binding_sites_of_expression (Syn.Cst.Expression.FieldAccess target)
      @ binding_sites_of_expression value
  | Syn.Cst.Expression.Assign { target; value; _ } ->
      binding_sites_of_expression target @ binding_sites_of_expression value
  | Syn.Cst.Expression.Infix { left; right; _ } ->
      binding_sites_of_expression left @ binding_sites_of_expression right
  | Syn.Cst.Expression.Typed { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } ->
      binding_sites_of_expression expression
  | Syn.Cst.Expression.Coerce { expression; _ } ->
      binding_sites_of_expression expression
  | Syn.Cst.Expression.Sequence { left; right; _ } ->
      binding_sites_of_expression left @ binding_sites_of_expression right
  | Syn.Cst.Expression.Tuple { elements; _ }
  | Syn.Cst.Expression.List { elements; _ }
  | Syn.Cst.Expression.Array { elements; _ } ->
      elements |> List.concat_map binding_sites_of_expression
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
             binding_sites_of_expression field.value)
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) ->
      binding_sites_of_expression base
      @
      (fields
      |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
             binding_sites_of_expression field.value))
  | Syn.Cst.Expression.LocalOpen { body; _ } ->
      binding_sites_of_expression body
  | Syn.Cst.Expression.Fun { body; _ } ->
      binding_sites_of_function_body body
  | Syn.Cst.Expression.Function { cases; _ } ->
      cases |> List.concat_map binding_sites_of_match_case
  | Syn.Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
      binding_sites_of_expression binding.bound_value
      @
      (and_bindings
      |> List.concat_map (fun ({ bound_value; _ } : Syn.Cst.binding_operator_binding) ->
             binding_sites_of_expression bound_value))
      @ binding_sites_of_expression body
  | Syn.Cst.Expression.Let
      { syntax_node; binding_pattern; bound_value; and_bindings; body; _ } ->
      Option.to_list
        (binding_site_of_expression_let ~syntax_node ~binding_pattern ~bound_value)
      @ binding_sites_of_expression bound_value
      @ (and_bindings |> List.concat_map binding_sites_of_let_binding)
      @ binding_sites_of_expression body
  | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
      binding_sites_of_expression scrutinee
      @ (cases |> List.concat_map binding_sites_of_match_case)
  | Syn.Cst.Expression.Try { body; cases; _ } ->
      binding_sites_of_expression body
      @ (cases |> List.concat_map binding_sites_of_match_case)
  | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      binding_sites_of_expression condition
      @ binding_sites_of_expression then_branch
      @
      (Option.to_list else_branch |> List.concat_map binding_sites_of_expression)
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      binding_sites_of_expression inner

and binding_sites_of_apply_argument = function
  | Syn.Cst.Positional argument ->
      binding_sites_of_expression argument
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      Option.to_list value |> List.concat_map binding_sites_of_expression

and binding_sites_of_let_binding binding =
  Option.to_list (binding_site_of_let_binding binding)
  @ binding_sites_of_expression (Syn.Cst.LetBinding.value binding)

and binding_sites_of_match_case ({ guard; body; _ } : Syn.Cst.match_case) =
  (Option.to_list guard |> List.concat_map binding_sites_of_expression)
  @ binding_sites_of_expression body

and binding_sites_of_class_field = function
  | Syn.Cst.ClassField.Method { body; _ } ->
      Option.to_list body |> List.concat_map binding_sites_of_expression
  | Syn.Cst.ClassField.Value { value; _ } ->
      Option.to_list value |> List.concat_map binding_sites_of_expression
  | Syn.Cst.ClassField.Inherit { class_expression; _ } ->
      binding_sites_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ ->
      []
  | Syn.Cst.ClassField.Initializer { body; _ } ->
      Option.to_list body |> List.concat_map binding_sites_of_expression
  | Syn.Cst.ClassField.Attribute { field; _ } ->
      binding_sites_of_class_field field
  | Syn.Cst.ClassField.Extension _ ->
      []

and binding_sites_of_class_expression = function
  | Syn.Cst.ClassExpression.Path _ | Syn.Cst.ClassExpression.Extension _ ->
      []
  | Syn.Cst.ClassExpression.Structure { fields; _ } ->
      fields |> List.concat_map binding_sites_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } ->
      binding_sites_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } ->
      binding_sites_of_class_expression callee
      @ binding_sites_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let
      { syntax_node; binding_pattern; bound_value; and_bindings; body; _ } ->
      Option.to_list
        (binding_site_of_expression_let ~syntax_node ~binding_pattern ~bound_value)
      @ binding_sites_of_expression bound_value
      @ (and_bindings |> List.concat_map binding_sites_of_let_binding)
      @ binding_sites_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } ->
      binding_sites_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen { class_expression; _ } ->
      binding_sites_of_class_expression class_expression
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } ->
      binding_sites_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } ->
      binding_sites_of_class_expression class_expression

let binding_sites_of_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      binding_sites_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      binding_sites_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } ->
      Option.to_list class_body |> List.concat_map binding_sites_of_class_expression
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      Option.to_list (Syn.Cst.ModuleDeclaration.module_expression decl)
      |> List.concat_map binding_sites_of_module_expression
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
      Syn.Cst.RecursiveModuleDeclaration.declarations decl
      |> List.concat_map (fun nested_decl ->
             Option.to_list (Syn.Cst.ModuleDeclaration.module_expression nested_decl)
             |> List.concat_map binding_sites_of_module_expression)
  | Syn.Cst.StructureItem.OpenStatement stmt -> (
      match Syn.Cst.OpenStatement.module_expression stmt with
      | Some expr -> binding_sites_of_module_expression expr
      | None -> [])
  | Syn.Cst.StructureItem.IncludeStatement { target; _ } -> (
      match target with
      | Syn.Cst.ModuleExpression expr -> binding_sites_of_module_expression expr
      | Syn.Cst.ModuleType _ -> [])
  | Syn.Cst.StructureItem.TypeDeclaration _
  | Syn.Cst.StructureItem.TypeExtension _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _
  | Syn.Cst.StructureItem.ClassTypeDeclaration _
  | Syn.Cst.StructureItem.ModuleTypeDeclaration _
  | Syn.Cst.StructureItem.ValueDeclaration _
  | Syn.Cst.StructureItem.ExternalDeclaration _
  | Syn.Cst.StructureItem.ExceptionDeclaration _ ->
      []
