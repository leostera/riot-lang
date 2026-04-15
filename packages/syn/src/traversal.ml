open Std

let rec binding_operator_bindings_of_chain = fun (binding: Cst.binding_operator_binding) ->
  binding :: (
    match binding.and_binding with
    | Some next -> binding_operator_bindings_of_chain next
    | None -> []
  )

let expressions_of_apply_argument = function
  | Cst.Positional expression -> [ expression ]
  | Cst.Labeled { value; _ }
  | Cst.Optional { value; _ } -> Option.to_list value

let expressions_of_parameter = fun (_: Cst.Parameter.t) -> []

let rec expressions_of_object_member = function
  | Cst.ObjectMember.Method { body; _ } -> [ body ]
  | Cst.ObjectMember.Value { value; _ } -> [ value ]
  | Cst.ObjectMember.Inherit { expression; _ } -> [ expression ]
  | Cst.ObjectMember.Extension _ -> []
  | Cst.ObjectMember.Initializer { body; _ } -> [ body ]

and expressions_of_class_field = function
  | Cst.ClassField.Method { definition=Cst.ConcreteMethod { body; _ }; _ } -> [ body ]
  | Cst.ClassField.Method { definition=Cst.VirtualMethod _; _ } -> []
  | Cst.ClassField.Value { definition=Cst.ConcreteValue { value; _ }; _ } -> [ value ]
  | Cst.ClassField.Value { definition=Cst.VirtualValue _; _ } -> []
  | Cst.ClassField.Inherit _
  | Cst.ClassField.Constraint _
  | Cst.ClassField.Extension _ -> []
  | Cst.ClassField.Initializer { body; _ } -> [ body ]
  | Cst.ClassField.Attribute { field; _ } -> expressions_of_class_field field

and expressions_of_class_expression = function
  | Cst.ClassExpression.Path _
  | Cst.ClassExpression.Extension _ -> []
  | Cst.ClassExpression.Structure { fields; _ } -> fields
  |> List.map ~fn:expressions_of_class_field
  |> List.concat
  | Cst.ClassExpression.Fun { body; _ } -> expressions_of_class_expression body
  | Cst.ClassExpression.Apply { callee; argument; _ } -> expressions_of_class_expression callee
  @ expressions_of_apply_argument argument
  | Cst.ClassExpression.Let {
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } -> [ bound_value ]
  @ (Option.to_list and_binding
  |> List.map
    ~fn:(fun binding -> Cst.LetBinding.and_bindings binding |> List.map ~fn:Cst.LetBinding.value)
  |> List.concat)
  @ (parameters |> List.map ~fn:expressions_of_parameter |> List.concat)
  @ expressions_of_class_expression body
  | Cst.ClassExpression.Constraint { class_expression; _ } -> expressions_of_class_expression class_expression
  | Cst.ClassExpression.LocalOpen (Cst.LetOpen { body; _ })
  | Cst.ClassExpression.LocalOpen (Cst.Delimited { body; _ }) -> expressions_of_class_expression body
  | Cst.ClassExpression.Parenthesized { inner; _ } -> expressions_of_class_expression inner
  | Cst.ClassExpression.Attribute { class_expression; _ } -> expressions_of_class_expression class_expression

and expressions_of_module_expression = function
  | Cst.ModuleExpression.Path _
  | Cst.ModuleExpression.Structure _
  | Cst.ModuleExpression.Extension _ -> []
  | Cst.ModuleExpression.Functor { body; _ } -> expressions_of_module_expression body
  | Cst.ModuleExpression.Apply { callee; argument; _ } -> expressions_of_module_expression callee
  @ expressions_of_module_expression argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } -> expressions_of_module_expression callee
  | Cst.ModuleExpression.Constraint { module_expression; _ }
  | Cst.ModuleExpression.Attribute { module_expression; _ } -> expressions_of_module_expression module_expression
  | Cst.ModuleExpression.ModuleUnpack { expression; _ } -> [ expression ]
  | Cst.ModuleExpression.Parenthesized { inner; _ } -> expressions_of_module_expression inner

let children_of_expression = function
  | Cst.Expression.Path _
  | Cst.Expression.Operator _
  | Cst.Expression.Literal _
  | Cst.Expression.Unreachable _
  | Cst.Expression.Extension _
  | Cst.Expression.New _ ->
      []
  | Cst.Expression.Constructor { payload; _ } ->
      Option.to_list payload
  | Cst.Expression.Object { members; _ } ->
      members |> List.map ~fn:expressions_of_object_member |> List.concat
  | Cst.Expression.PolyVariant { payload; _ } ->
      Option.to_list payload
  | Cst.Expression.ModulePack { module_expression; _ } ->
      expressions_of_module_expression module_expression
  | Cst.Expression.LetModule { module_expression; body; _ } ->
      expressions_of_module_expression module_expression @ [ body ]
  | Cst.Expression.LetException { body; _ } ->
      [ body ]
  | Cst.Expression.Assert { asserted; _ } ->
      [ asserted ]
  | Cst.Expression.Lazy { body; _ } ->
      [ body ]
  | Cst.Expression.While { condition; body; _ } ->
      [ condition; body ]
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      [ start_expr; end_expr; body ]
  | Cst.Expression.Apply { callee; argument; _ } ->
      callee :: expressions_of_apply_argument argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      [ receiver ]
  | Cst.Expression.Prefix { operand; _ } ->
      [ operand ]
  | Cst.Expression.FieldAccess { receiver; _ } ->
      [ receiver ]
  | Cst.Expression.Index { collection; index; _ } ->
      [ collection; index ]
  | Cst.Expression.ObjectOverride { fields; _ } ->
      fields
      |> List.map ~fn:(fun (field: Cst.object_override_field) -> Option.to_list field.value)
      |> List.concat
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      [ value ]
  | Cst.Expression.FieldAssign { target; value; _ } ->
      [ Cst.Expression.FieldAccess target; value ]
  | Cst.Expression.Assign { target; value; _ } ->
      [ target; value ]
  | Cst.Expression.Infix { left; right; _ } ->
      [ left; right ]
  | Cst.Expression.TypeAscription { expression; _ }
  | Cst.Expression.Polymorphic { expression; _ } ->
      [ expression ]
  | Cst.Expression.Sequence { expressions; _ } ->
      expressions
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      elements
  | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
      fields |> List.map ~fn:(fun (field: Cst.record_expression_field) -> field.value)
  | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
      base :: List.map fields ~fn:(fun (field: Cst.record_expression_field) -> field.value)
  | Cst.Expression.LocalOpen (Cst.LetOpen { body; _ })
  | Cst.Expression.LocalOpen (Cst.Delimited { body; _ }) ->
      [ body ]
  | Cst.Expression.Fun { body; _ } -> (
      match body with
      | Cst.Expression expression -> [ expression ]
      | Cst.Cases { cases; _ } -> cases
      |> List.map ~fn:(fun ({ guard; body; _ }: Cst.match_case) -> Option.to_list guard @ [ body ])
      |> List.concat
    )
  | Cst.Expression.Function { cases; _ } ->
      cases
      |> List.map ~fn:(fun ({ guard; body; _ }: Cst.match_case) -> Option.to_list guard @ [ body ])
      |> List.concat
  | Cst.Expression.LetOperator { binding; body; _ } ->
      (binding_operator_bindings_of_chain binding
      |> List.map ~fn:(fun ({ bound_value; _ }: Cst.binding_operator_binding) -> bound_value))
      @ [ body ]
  | Cst.Expression.Let {
    parameters;
    bound_value;
    and_binding;
    body;
    _
  } ->
      [ bound_value ]
      @ (Option.to_list and_binding
      |> List.map
        ~fn:(fun binding -> Cst.LetBinding.and_bindings binding |> List.map ~fn:Cst.LetBinding.value)
      |> List.concat)
      @ (parameters |> List.map ~fn:expressions_of_parameter |> List.concat)
      @ [ body ]
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      [ scrutinee ]
      @ (cases
      |> List.map ~fn:(fun ({ guard; body; _ }: Cst.match_case) -> Option.to_list guard @ [ body ])
      |> List.concat)
  | Cst.Expression.Try { body; cases; _ } ->
      [ body ]
      @ (cases
      |> List.map ~fn:(fun ({ guard; body; _ }: Cst.match_case) -> Option.to_list guard @ [ body ])
      |> List.concat)
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      [ condition; then_branch ] @ Option.to_list else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      [ inner ]

let rec fold_expression = fun f acc expr ->
  let acc = f acc expr in
  children_of_expression expr |> List.fold_left ~acc ~fn:(fold_expression f)

let iter_expression = fun f expr ->
  fold_expression
    (fun () expression ->
      f expression;
      ())
    ()
    expr

let exists_expression = fun predicate expr ->
  let found = ref false in
  let rec go expression =
    if not !found then
      (
        if predicate expression then
          found := true
        else
          children_of_expression expression |> List.for_each ~fn:go
      )
  in
  go expr;
  !found

let children_of_core_type = function
  | Cst.CoreType.Wildcard _
  | Cst.CoreType.Var _
  | Cst.CoreType.Extension _ -> []
  | Cst.CoreType.Constr { arguments; _ }
  | Cst.CoreType.Class { arguments; _ } -> arguments
  | Cst.CoreType.Alias { type_; _ }
  | Cst.CoreType.Attribute { type_; _ }
  | Cst.CoreType.Parenthesized { inner=type_; _ } -> [ type_ ]
  | Cst.CoreType.Poly { body; _ } -> [ body ]
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } -> [ parameter_type; result_type ]
  | Cst.CoreType.Tuple { elements; _ } -> elements
  | Cst.CoreType.PolyVariant { fields; _ } ->
      fields |> List.map
        ~fn:(
          function
          | Cst.RowField.Tag tag -> Option.to_list tag.payload_type
          | Cst.RowField.Inherit { type_; _ } -> [ type_ ]
        ) |> List.concat
  | Cst.CoreType.Record { fields; _ } -> fields
  |> List.map ~fn:(fun (field: Cst.record_type_field) -> field.field_type)
  | Cst.CoreType.FirstClassModule _
  | Cst.CoreType.Object _ -> []

let rec fold_core_type = fun f acc type_ ->
  let acc = f acc type_ in
  children_of_core_type type_ |> List.fold_left ~acc ~fn:(fold_core_type f)

let iter_core_type = fun f type_ ->
  fold_core_type
    (fun () core_type ->
      f core_type;
      ())
    ()
    type_

let exists_core_type = fun predicate type_ ->
  let found = ref false in
  let rec go core_type =
    if not !found then
      (
        if predicate core_type then
          found := true
        else
          children_of_core_type core_type |> List.for_each ~fn:go
      )
  in
  go type_;
  !found
