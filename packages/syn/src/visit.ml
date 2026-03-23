open Std

type 'ctx control =
  | Continue of 'ctx
  | Skip_children of 'ctx
  | Stop of 'ctx

type 'ctx visitor = {
  enter_structure_item :
    'ctx -> Cst.StructureItem.t -> 'ctx control;
  enter_signature_item :
    'ctx -> Cst.SignatureItem.t -> 'ctx control;
  enter_let_binding :
    'ctx -> Cst.LetBinding.t -> 'ctx control;
  enter_type_declaration :
    'ctx -> Cst.TypeDeclaration.t -> 'ctx control;
  enter_expression :
    'ctx -> Cst.Expression.t -> 'ctx control;
  enter_core_type :
    'ctx -> Cst.CoreType.t -> 'ctx control;
}

let default =
  {
    enter_structure_item = (fun ctx _ -> Continue ctx);
    enter_signature_item = (fun ctx _ -> Continue ctx);
    enter_let_binding = (fun ctx _ -> Continue ctx);
    enter_type_declaration = (fun ctx _ -> Continue ctx);
    enter_expression = (fun ctx _ -> Continue ctx);
    enter_core_type = (fun ctx _ -> Continue ctx);
  }

let bind_control f control =
  match control with
  | Continue ctx ->
      f ctx
  | Skip_children ctx ->
      Continue ctx
  | Stop ctx ->
      Stop ctx

let fold_controls f init values =
  let rec go ctx = function
    | [] ->
        Continue ctx
    | value :: rest -> (
        match f ctx value with
        | Continue ctx' | Skip_children ctx' ->
            go ctx' rest
        | Stop ctx' ->
            Stop ctx')
  in
  go init values

let rec visit_module_type visitor ctx = function
  | Cst.ModuleType.Path _
  | Cst.ModuleType.TypeOf _
  | Cst.ModuleType.Signature _
  | Cst.ModuleType.Extension _ ->
      Continue ctx
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      fold_controls
        (fun ctx (parameter : Cst.functor_parameter) ->
          visit_module_type visitor ctx parameter.module_type)
        ctx parameters
      |> bind_control (fun ctx -> visit_module_type visitor ctx result)
  | Cst.ModuleType.With { base; constraints; _ } ->
      visit_module_type visitor ctx base
      |> bind_control (fun ctx ->
             fold_controls
               (fun ctx (constraint_ : Cst.module_type_constraint) ->
                 visit_core_type visitor ctx constraint_.constrained_type
                 |> bind_control (fun ctx ->
                        visit_core_type visitor ctx constraint_.replacement_type))
               ctx constraints)
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      visit_module_type visitor ctx inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      visit_module_type visitor ctx module_type

and visit_class_type visitor ctx = function
  | Cst.ClassType.Path _ | Cst.ClassType.Extension _ ->
      Continue ctx
  | Cst.ClassType.Signature { fields; _ } ->
      fold_controls (visit_class_type_field visitor) ctx fields
  | Cst.ClassType.Arrow { parameter_type; result_type; _ } ->
      visit_core_type visitor ctx parameter_type
      |> bind_control (fun ctx -> visit_class_type visitor ctx result_type)
  | Cst.ClassType.Parenthesized { inner; _ } ->
      visit_class_type visitor ctx inner
  | Cst.ClassType.LocalOpen { class_type; _ } ->
      visit_class_type visitor ctx class_type
  | Cst.ClassType.Attribute { class_type; _ } ->
      visit_class_type visitor ctx class_type

and visit_class_type_field visitor ctx = function
  | Cst.ClassTypeField.Inherit { class_type; _ } ->
      visit_class_type visitor ctx class_type
  | Cst.ClassTypeField.Value { type_; _ }
  | Cst.ClassTypeField.Method { type_; _ } ->
      visit_core_type visitor ctx type_
  | Cst.ClassTypeField.Constraint { left; right; _ } ->
      visit_core_type visitor ctx left
      |> bind_control (fun ctx -> visit_core_type visitor ctx right)
  | Cst.ClassTypeField.Attribute { field; _ } ->
      visit_class_type_field visitor ctx field
  | Cst.ClassTypeField.Extension _ ->
      Continue ctx

and visit_type_definition visitor ctx = function
  | Cst.TypeDefinition.Abstract
  | Cst.TypeDefinition.Extensible _ ->
      Continue ctx
  | Cst.TypeDefinition.Alias { manifest; _ } ->
      visit_core_type visitor ctx manifest
  | Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
      visit_module_type visitor ctx module_type
  | Cst.TypeDefinition.Object { fields; _ } ->
      fold_controls
        (fun ctx (field : Cst.object_type_field) ->
          visit_core_type visitor ctx field.field_type)
        ctx fields
  | Cst.TypeDefinition.Record { fields; _ } ->
      fold_controls
        (fun ctx (field : Cst.RecordField.t) ->
          visit_core_type visitor ctx (Cst.RecordField.field_type field))
        ctx fields
  | Cst.TypeDefinition.Variant { constructors; _ } ->
      fold_controls (visit_variant_constructor visitor) ctx constructors
  | Cst.TypeDefinition.PolyVariant poly_variant ->
      fold_controls
        (fun ctx row_field ->
          match row_field with
          | Cst.RowField.Tag tag -> (
              match tag.payload_type with
              | Some payload_type ->
                  visit_core_type visitor ctx payload_type
              | None ->
                  Continue ctx)
          | Cst.RowField.Inherit { type_; _ } ->
              visit_core_type visitor ctx type_)
        ctx (Cst.PolyVariant.fields poly_variant)

and visit_variant_constructor visitor ctx (constructor : Cst.VariantConstructor.t) =
  let argument_types =
    match Cst.VariantConstructor.arguments constructor with
    | Some (Cst.ConstructorArguments.Tuple elements) ->
        elements
    | Some (Cst.ConstructorArguments.Record fields) ->
        fields |> List.map Cst.RecordField.field_type
    | None ->
        []
  in
  fold_controls (visit_core_type visitor) ctx argument_types
  |> bind_control (fun ctx ->
         match Cst.VariantConstructor.payload_type constructor with
         | Some payload_type ->
             visit_core_type visitor ctx payload_type
         | None ->
             Continue ctx)
  |> bind_control (fun ctx ->
         match Cst.VariantConstructor.result_type constructor with
         | Some result_type ->
             visit_core_type visitor ctx result_type
         | None ->
             Continue ctx)

and visit_module_expression visitor ctx = function
  | Cst.ModuleExpression.Path _
  | Cst.ModuleExpression.Structure _
  | Cst.ModuleExpression.Extension _ ->
      Continue ctx
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      fold_controls
        (fun ctx (parameter : Cst.functor_parameter) ->
          visit_module_type visitor ctx parameter.module_type)
        ctx parameters
      |> bind_control (fun ctx -> visit_module_expression visitor ctx body)
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      visit_module_expression visitor ctx callee
      |> bind_control (fun ctx -> visit_module_expression visitor ctx argument)
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      visit_module_expression visitor ctx callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      visit_module_expression visitor ctx module_expression
      |> bind_control (fun ctx -> visit_module_type visitor ctx module_type)
  | Cst.ModuleExpression.ModuleUnpack { expression; module_type; _ } ->
      visit_expression visitor ctx expression
      |> bind_control (fun ctx ->
             match module_type with
             | Some module_type ->
                 visit_module_type visitor ctx module_type
             | None ->
                 Continue ctx)
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      visit_module_expression visitor ctx inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      visit_module_expression visitor ctx module_expression

and visit_class_field visitor ctx = function
  | Cst.ClassField.Method { body; type_; _ } ->
      (match body with
      | Some body -> visit_expression visitor ctx body
      | None -> Continue ctx)
      |> bind_control (fun ctx ->
             match type_ with
             | Some type_ -> visit_core_type visitor ctx type_
             | None -> Continue ctx)
  | Cst.ClassField.Value { value; type_; _ } ->
      (match value with
      | Some value -> visit_expression visitor ctx value
      | None -> Continue ctx)
      |> bind_control (fun ctx ->
             match type_ with
             | Some type_ -> visit_core_type visitor ctx type_
             | None -> Continue ctx)
  | Cst.ClassField.Inherit { class_expression; _ } ->
      visit_class_expression visitor ctx class_expression
  | Cst.ClassField.Constraint { left; right; _ } ->
      visit_core_type visitor ctx left
      |> bind_control (fun ctx -> visit_core_type visitor ctx right)
  | Cst.ClassField.Initializer { body; _ } -> (
      match body with
      | Some body ->
          visit_expression visitor ctx body
      | None ->
          Continue ctx)
  | Cst.ClassField.Attribute { field; _ } ->
      visit_class_field visitor ctx field
  | Cst.ClassField.Extension _ ->
      Continue ctx

and visit_class_expression visitor ctx = function
  | Cst.ClassExpression.Path _ | Cst.ClassExpression.Extension _ ->
      Continue ctx
  | Cst.ClassExpression.Structure { fields; _ } ->
      fold_controls (visit_class_field visitor) ctx fields
  | Cst.ClassExpression.Fun { body; _ } ->
      visit_class_expression visitor ctx body
  | Cst.ClassExpression.Apply { callee; argument; _ } ->
      visit_class_expression visitor ctx callee
      |> bind_control (fun ctx ->
             match argument with
             | Cst.Positional expression ->
                 visit_expression visitor ctx expression
             | Cst.Labeled { value; _ } | Cst.Optional { value; _ } -> (
                 match value with
                 | Some value ->
                     visit_expression visitor ctx value
                 | None ->
                     Continue ctx))
  | Cst.ClassExpression.Let { bound_value; and_bindings; body; _ } ->
      visit_expression visitor ctx bound_value
      |> bind_control (fun ctx ->
             fold_controls (visit_let_binding visitor) ctx and_bindings)
      |> bind_control (fun ctx -> visit_class_expression visitor ctx body)
  | Cst.ClassExpression.Constraint { class_expression; class_type; _ } ->
      visit_class_expression visitor ctx class_expression
      |> bind_control (fun ctx -> visit_class_type visitor ctx class_type)
  | Cst.ClassExpression.LocalOpen { class_expression; _ } ->
      visit_class_expression visitor ctx class_expression
  | Cst.ClassExpression.Parenthesized { inner; _ } ->
      visit_class_expression visitor ctx inner
  | Cst.ClassExpression.Attribute { class_expression; _ } ->
      visit_class_expression visitor ctx class_expression

and visit_expression visitor ctx expression =
  match visitor.enter_expression ctx expression with
  | Stop ctx ->
      Stop ctx
  | Skip_children ctx ->
      Continue ctx
  | Continue ctx -> (
      match expression with
      | Cst.Expression.Path _
      | Cst.Expression.Operator _
      | Cst.Expression.Literal _
      | Cst.Expression.Unreachable _
      | Cst.Expression.Extension _
      | Cst.Expression.New _ ->
          Continue ctx
      | Cst.Expression.Constructor { payload; _ } -> (
          match payload with
          | Some payload ->
              visit_expression visitor ctx payload
          | None ->
              Continue ctx)
      | Cst.Expression.Object { members; _ } ->
          fold_controls
            (fun ctx member ->
              match member with
              | Cst.ObjectMember.Method { body; _ }
              | Cst.ObjectMember.Value { value = body; _ }
              | Cst.ObjectMember.Initializer { body; _ } -> (
                  match body with
                  | Some body ->
                      visit_expression visitor ctx body
                  | None ->
                      Continue ctx)
              | Cst.ObjectMember.Inherit { expression; _ } ->
                  visit_expression visitor ctx expression
              | Cst.ObjectMember.Extension _ ->
                  Continue ctx)
            ctx members
      | Cst.Expression.PolyVariant { payload; _ } -> (
          match payload with
          | Some payload ->
              visit_expression visitor ctx payload
          | None ->
              Continue ctx)
      | Cst.Expression.ModulePack { module_expression; _ } ->
          visit_module_expression visitor ctx module_expression
      | Cst.Expression.LetModule { module_expression; body; _ } ->
          visit_module_expression visitor ctx module_expression
          |> bind_control (fun ctx -> visit_expression visitor ctx body)
      | Cst.Expression.LetException { body; _ } ->
          visit_expression visitor ctx body
      | Cst.Expression.Assert { asserted; _ } ->
          visit_expression visitor ctx asserted
      | Cst.Expression.Lazy { body; _ } ->
          visit_expression visitor ctx body
      | Cst.Expression.While { condition; body; _ } ->
          visit_expression visitor ctx condition
          |> bind_control (fun ctx -> visit_expression visitor ctx body)
      | Cst.Expression.For { start_expr; end_expr; body; _ } ->
          visit_expression visitor ctx start_expr
          |> bind_control (fun ctx -> visit_expression visitor ctx end_expr)
          |> bind_control (fun ctx -> visit_expression visitor ctx body)
      | Cst.Expression.Apply { callee; argument; _ } ->
          visit_expression visitor ctx callee
          |> bind_control (fun ctx ->
                 match argument with
                 | Cst.Positional expression ->
                     visit_expression visitor ctx expression
                 | Cst.Labeled { value; _ } | Cst.Optional { value; _ } -> (
                     match value with
                     | Some value ->
                         visit_expression visitor ctx value
                     | None ->
                         Continue ctx))
      | Cst.Expression.MethodCall { receiver; _ } ->
          visit_expression visitor ctx receiver
      | Cst.Expression.Prefix { operand; _ } ->
          visit_expression visitor ctx operand
      | Cst.Expression.FieldAccess { receiver; _ } ->
          visit_expression visitor ctx receiver
      | Cst.Expression.Index { collection; index; _ } ->
          visit_expression visitor ctx collection
          |> bind_control (fun ctx -> visit_expression visitor ctx index)
      | Cst.Expression.ObjectOverride { fields; _ } ->
          fold_controls
            (fun ctx (field : Cst.object_override_field) ->
              match field.value with
              | Some value ->
                  visit_expression visitor ctx value
              | None ->
                  Continue ctx)
            ctx fields
      | Cst.Expression.InstanceVariableAssign { value; _ } ->
          visit_expression visitor ctx value
      | Cst.Expression.FieldAssign { target; value; _ } ->
          visit_expression visitor ctx (Cst.Expression.FieldAccess target)
          |> bind_control (fun ctx -> visit_expression visitor ctx value)
      | Cst.Expression.Assign { target; value; _ } ->
          visit_expression visitor ctx target
          |> bind_control (fun ctx -> visit_expression visitor ctx value)
      | Cst.Expression.Infix { left; right; _ }
      | Cst.Expression.Sequence { left; right; _ } ->
          visit_expression visitor ctx left
          |> bind_control (fun ctx -> visit_expression visitor ctx right)
      | Cst.Expression.Typed { expression; type_; _ }
      | Cst.Expression.Polymorphic { expression; type_; _ } ->
          visit_expression visitor ctx expression
          |> bind_control (fun ctx -> visit_core_type visitor ctx type_)
      | Cst.Expression.Coerce { expression; from_type; to_type; _ } ->
          visit_expression visitor ctx expression
          |> bind_control (fun ctx ->
                 match from_type with
                 | Some from_type ->
                     visit_core_type visitor ctx from_type
                 | None ->
                     Continue ctx)
          |> bind_control (fun ctx -> visit_core_type visitor ctx to_type)
      | Cst.Expression.Tuple { elements; _ }
      | Cst.Expression.List { elements; _ }
      | Cst.Expression.Array { elements; _ } ->
          fold_controls (visit_expression visitor) ctx elements
      | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
          fold_controls
            (fun ctx (field : Cst.record_expression_field) ->
              visit_expression visitor ctx field.value)
            ctx fields
      | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
          visit_expression visitor ctx base
          |> bind_control (fun ctx ->
                 fold_controls
                   (fun ctx (field : Cst.record_expression_field) ->
                     visit_expression visitor ctx field.value)
                   ctx fields)
      | Cst.Expression.LocalOpen { body; _ } ->
          visit_expression visitor ctx body
      | Cst.Expression.Fun { body; _ } -> (
          match body with
          | Cst.Expression expression ->
              visit_expression visitor ctx expression
          | Cst.Cases { cases; _ } ->
              fold_controls
                (fun ctx ({ guard; body; _ } : Cst.match_case) ->
                  (match guard with
                  | Some guard ->
                      visit_expression visitor ctx guard
                  | None ->
                      Continue ctx)
                  |> bind_control (fun ctx -> visit_expression visitor ctx body))
                ctx cases)
      | Cst.Expression.Function { cases; _ } ->
          fold_controls
            (fun ctx ({ guard; body; _ } : Cst.match_case) ->
              (match guard with
              | Some guard ->
                  visit_expression visitor ctx guard
              | None ->
                  Continue ctx)
              |> bind_control (fun ctx -> visit_expression visitor ctx body))
            ctx cases
      | Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
          visit_expression visitor ctx binding.bound_value
          |> bind_control (fun ctx ->
                 fold_controls
                   (fun ctx ({ bound_value; _ } : Cst.binding_operator_binding) ->
                     visit_expression visitor ctx bound_value)
                   ctx and_bindings)
          |> bind_control (fun ctx -> visit_expression visitor ctx body)
      | Cst.Expression.Let { bound_value; and_bindings; body; _ } ->
          visit_expression visitor ctx bound_value
          |> bind_control (fun ctx ->
                 fold_controls (visit_let_binding visitor) ctx and_bindings)
          |> bind_control (fun ctx -> visit_expression visitor ctx body)
      | Cst.Expression.Match { scrutinee; cases; _ } ->
          visit_expression visitor ctx scrutinee
          |> bind_control (fun ctx ->
                 fold_controls
                   (fun ctx ({ guard; body; _ } : Cst.match_case) ->
                     (match guard with
                     | Some guard ->
                         visit_expression visitor ctx guard
                     | None ->
                         Continue ctx)
                     |> bind_control (fun ctx -> visit_expression visitor ctx body))
                   ctx cases)
      | Cst.Expression.Try { body; cases; _ } ->
          visit_expression visitor ctx body
          |> bind_control (fun ctx ->
                 fold_controls
                   (fun ctx ({ guard; body; _ } : Cst.match_case) ->
                     (match guard with
                     | Some guard ->
                         visit_expression visitor ctx guard
                     | None ->
                         Continue ctx)
                     |> bind_control (fun ctx -> visit_expression visitor ctx body))
                   ctx cases)
      | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
          visit_expression visitor ctx condition
          |> bind_control (fun ctx -> visit_expression visitor ctx then_branch)
          |> bind_control (fun ctx ->
                 match else_branch with
                 | Some else_branch ->
                     visit_expression visitor ctx else_branch
                 | None ->
                     Continue ctx)
      | Cst.Expression.Parenthesized { inner; _ } ->
          visit_expression visitor ctx inner)

and visit_core_type visitor ctx core_type =
  match visitor.enter_core_type ctx core_type with
  | Stop ctx ->
      Stop ctx
  | Skip_children ctx ->
      Continue ctx
  | Continue ctx -> (
      match core_type with
      | Cst.CoreType.Wildcard _
      | Cst.CoreType.Var _
      | Cst.CoreType.Extension _ ->
          Continue ctx
      | Cst.CoreType.Constr { arguments; _ }
      | Cst.CoreType.Class { arguments; _ } ->
          fold_controls (visit_core_type visitor) ctx arguments
      | Cst.CoreType.Alias { type_; _ }
      | Cst.CoreType.Attribute { type_; _ }
      | Cst.CoreType.Parenthesized { inner = type_; _ }
      | Cst.CoreType.LocalOpen { type_; _ } ->
          visit_core_type visitor ctx type_
      | Cst.CoreType.Poly { body; _ } ->
          visit_core_type visitor ctx body
      | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
          visit_core_type visitor ctx parameter_type
          |> bind_control (fun ctx -> visit_core_type visitor ctx result_type)
      | Cst.CoreType.Tuple { elements; _ } ->
          fold_controls (visit_core_type visitor) ctx elements
      | Cst.CoreType.PolyVariant poly_variant ->
          fold_controls
            (fun ctx row_field ->
              match row_field with
              | Cst.RowField.Tag tag -> (
                  match tag.payload_type with
                  | Some payload_type ->
                      visit_core_type visitor ctx payload_type
                  | None ->
                      Continue ctx)
              | Cst.RowField.Inherit { type_; _ } ->
                  visit_core_type visitor ctx type_)
            ctx (Cst.PolyVariant.fields poly_variant)
      | Cst.CoreType.Record { fields; _ } ->
          fold_controls
            (fun ctx (field : Cst.record_type_field) ->
              visit_core_type visitor ctx field.field_type)
            ctx fields
      | Cst.CoreType.FirstClassModule { module_type; _ } ->
          visit_module_type visitor ctx module_type
      | Cst.CoreType.Object { fields; _ } ->
          fold_controls
            (fun ctx (field : Cst.object_type_field) ->
              visit_core_type visitor ctx field.field_type)
            ctx fields)

and visit_let_binding visitor ctx binding =
  match visitor.enter_let_binding ctx binding with
  | Stop ctx ->
      Stop ctx
  | Skip_children ctx ->
      Continue ctx
  | Continue ctx ->
      visit_expression visitor ctx (Cst.LetBinding.value binding)

and visit_type_declaration visitor ctx declaration =
  match visitor.enter_type_declaration ctx declaration with
  | Stop ctx ->
      Stop ctx
  | Skip_children ctx ->
      Continue ctx
  | Continue ctx ->
      visit_type_definition visitor ctx (Cst.TypeDeclaration.type_definition declaration)
      |> bind_control (fun ctx ->
             fold_controls
               (fun ctx (constraint_ : Cst.TypeConstraint.t) ->
                 visit_core_type visitor ctx constraint_.left
                 |> bind_control (fun ctx -> visit_core_type visitor ctx constraint_.right))
               ctx (Cst.TypeDeclaration.constraints declaration))

let structure_item visitor ctx item =
  let rec visit_structure_item ctx item =
    match visitor.enter_structure_item ctx item with
    | Stop ctx ->
        Stop ctx
    | Skip_children ctx ->
        Continue ctx
    | Continue ctx -> (
        match item with
        | Cst.StructureItem.TypeDeclaration declaration ->
            visit_type_declaration visitor ctx declaration
        | Cst.StructureItem.TypeExtension declaration ->
            fold_controls (visit_variant_constructor visitor) ctx
              (Cst.TypeExtension.constructors declaration)
        | Cst.StructureItem.LetBinding binding ->
            visit_let_binding visitor ctx binding
        | Cst.StructureItem.Expression expression ->
            visit_expression visitor ctx expression
        | Cst.StructureItem.ClassDeclaration declaration ->
            (match declaration.class_type with
            | Some class_type ->
                visit_class_type visitor ctx class_type
            | None ->
                Continue ctx)
            |> bind_control (fun ctx ->
                   match declaration.class_body with
                   | Some class_body ->
                       visit_class_expression visitor ctx class_body
                   | None ->
                       Continue ctx)
        | Cst.StructureItem.ClassTypeDeclaration declaration ->
            visit_class_type visitor ctx declaration.class_type_body
        | Cst.StructureItem.ModuleDeclaration declaration ->
            (match Cst.ModuleDeclaration.module_type declaration with
            | Some module_type ->
                visit_module_type visitor ctx module_type
            | None ->
                Continue ctx)
            |> bind_control (fun ctx ->
                   match Cst.ModuleDeclaration.module_expression declaration with
                   | Some module_expression ->
                       visit_module_expression visitor ctx module_expression
                   | None ->
                       Continue ctx)
        | Cst.StructureItem.RecursiveModuleDeclaration declaration ->
            fold_controls
              (fun ctx module_declaration ->
                visit_structure_item ctx
                  (Cst.StructureItem.ModuleDeclaration module_declaration))
              ctx (Cst.RecursiveModuleDeclaration.declarations declaration)
        | Cst.StructureItem.ModuleTypeDeclaration declaration -> (
            match Cst.ModuleTypeDeclaration.module_type declaration with
            | Some module_type ->
                visit_module_type visitor ctx module_type
            | None ->
                Continue ctx)
        | Cst.StructureItem.OpenStatement statement -> (
            match Cst.OpenStatement.target statement with
            | Cst.OpenStatement.ModuleExpression module_expression ->
                visit_module_expression visitor ctx module_expression
            | Cst.OpenStatement.Path _ ->
                Continue ctx)
        | Cst.StructureItem.ValueDeclaration declaration ->
            visit_core_type visitor ctx declaration.type_
        | Cst.StructureItem.ExternalDeclaration declaration ->
            visit_core_type visitor ctx declaration.type_
        | Cst.StructureItem.IncludeStatement statement -> (
            match statement.target with
            | Cst.ModuleExpression module_expression ->
                visit_module_expression visitor ctx module_expression
            | Cst.ModuleType module_type ->
                visit_module_type visitor ctx module_type)
        | Cst.StructureItem.Attribute _ | Cst.StructureItem.Extension _
        | Cst.StructureItem.ExceptionDeclaration _ ->
            Continue ctx)
  in
  match visit_structure_item ctx item with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx

let signature_item visitor ctx item =
  let rec visit_signature_item ctx item =
    match visitor.enter_signature_item ctx item with
    | Stop ctx ->
        Stop ctx
    | Skip_children ctx ->
        Continue ctx
    | Continue ctx -> (
        match item with
        | Cst.SignatureItem.TypeDeclaration declaration ->
            visit_type_declaration visitor ctx declaration
        | Cst.SignatureItem.TypeExtension declaration ->
            fold_controls (visit_variant_constructor visitor) ctx
              (Cst.TypeExtension.constructors declaration)
        | Cst.SignatureItem.ClassDeclaration declaration ->
            (match declaration.class_type with
            | Some class_type ->
                visit_class_type visitor ctx class_type
            | None ->
                Continue ctx)
            |> bind_control (fun ctx ->
                   match declaration.class_body with
                   | Some class_body ->
                       visit_class_expression visitor ctx class_body
                   | None ->
                       Continue ctx)
        | Cst.SignatureItem.ClassTypeDeclaration declaration ->
            visit_class_type visitor ctx declaration.class_type_body
        | Cst.SignatureItem.ModuleDeclaration declaration ->
            (match Cst.ModuleDeclaration.module_type declaration with
            | Some module_type ->
                visit_module_type visitor ctx module_type
            | None ->
                Continue ctx)
            |> bind_control (fun ctx ->
                   match Cst.ModuleDeclaration.module_expression declaration with
                   | Some module_expression ->
                       visit_module_expression visitor ctx module_expression
                   | None ->
                       Continue ctx)
        | Cst.SignatureItem.RecursiveModuleDeclaration declaration ->
            fold_controls
              (fun ctx module_declaration ->
                visit_signature_item ctx
                  (Cst.SignatureItem.ModuleDeclaration module_declaration))
              ctx (Cst.RecursiveModuleDeclaration.declarations declaration)
        | Cst.SignatureItem.ModuleTypeDeclaration declaration -> (
            match Cst.ModuleTypeDeclaration.module_type declaration with
            | Some module_type ->
                visit_module_type visitor ctx module_type
            | None ->
                Continue ctx)
        | Cst.SignatureItem.OpenStatement statement -> (
            match Cst.OpenStatement.target statement with
            | Cst.OpenStatement.ModuleExpression module_expression ->
                visit_module_expression visitor ctx module_expression
            | Cst.OpenStatement.Path _ ->
                Continue ctx)
        | Cst.SignatureItem.ValueDeclaration declaration ->
            visit_core_type visitor ctx declaration.type_
        | Cst.SignatureItem.IncludeStatement statement -> (
            match statement.target with
            | Cst.ModuleExpression module_expression ->
                visit_module_expression visitor ctx module_expression
            | Cst.ModuleType module_type ->
                visit_module_type visitor ctx module_type)
        | Cst.SignatureItem.Attribute _ | Cst.SignatureItem.Extension _
        | Cst.SignatureItem.ExceptionDeclaration _ ->
            Continue ctx)
  in
  match visit_signature_item ctx item with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx

let source_file visitor ctx = function
  | Cst.Implementation implementation ->
      implementation.items
      |> List.fold_left (structure_item visitor) ctx
  | Cst.Interface interface ->
      interface.items
      |> List.fold_left (signature_item visitor) ctx

let let_binding visitor ctx binding =
  match visit_let_binding visitor ctx binding with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx

let type_declaration visitor ctx declaration =
  match visit_type_declaration visitor ctx declaration with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx

let expression visitor ctx expression =
  match visit_expression visitor ctx expression with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx

let core_type visitor ctx core_type =
  match visit_core_type visitor ctx core_type with
  | Continue ctx | Skip_children ctx | Stop ctx ->
      ctx
