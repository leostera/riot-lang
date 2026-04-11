open Std
module Core = Raml_core.Core_ir

let has_name = fun names name ->
  List.exists (String.equal name) names

let add_unique_name = fun names name ->
  if has_name names name then
    names
  else
    names @ [ name ]

let param_names = fun params ->
  List.map (fun (param: Core.Expr.param) -> param.name) params

let rec free_vars = fun ~name_of_entity ~bound expr ->
  match expr with
  | Core.Expr.Constant _ ->
      []
  | Core.Expr.Var entity_id -> (
      match name_of_entity entity_id with
      | Some name when not (has_name bound name) -> [ name ]
      | _ -> []
    )
  | Core.Expr.Apply { callee=Core.Expr.Direct _; arguments } ->
      List.fold_left
        (fun names argument ->
          List.fold_left add_unique_name names (free_vars ~name_of_entity ~bound argument))
        []
        arguments
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      List.fold_left
        (fun names expr ->
          List.fold_left add_unique_name names (free_vars ~name_of_entity ~bound expr))
        (free_vars ~name_of_entity ~bound callee)
        arguments
  | Core.Expr.Lambda lambda ->
      free_vars
        ~name_of_entity
        ~bound:(List.fold_left add_unique_name bound (param_names lambda.params))
        lambda.body
  | Core.Expr.Let let_ ->
      let binding_names =
        List.map (fun (binding: Core.Expr.binding) -> binding.name) let_.bindings
      in
      let binding_scope =
        match let_.rec_flag with
        | Core.Rec_flag.Nonrecursive -> bound
        | Core.Rec_flag.Recursive -> List.fold_left add_unique_name bound binding_names
      in
      let binding_free_vars =
        List.fold_left
          (fun names (binding: Core.Expr.binding) ->
            List.fold_left
              add_unique_name
              names
              (free_vars ~name_of_entity ~bound:binding_scope binding.expr))
          []
          let_.bindings
      in
      List.fold_left
        add_unique_name
        binding_free_vars
        (free_vars ~name_of_entity ~bound:(List.fold_left add_unique_name bound binding_names) let_.body)
  | Core.Expr.Sequence sequence ->
      List.fold_left
        add_unique_name
        (free_vars ~name_of_entity ~bound sequence.first)
        (free_vars ~name_of_entity ~bound sequence.second)
  | Core.Expr.Tuple tuple ->
      List.fold_left
        (fun names expr ->
          List.fold_left add_unique_name names (free_vars ~name_of_entity ~bound expr))
        []
        tuple
  | Core.Expr.Tuple_get tuple_get ->
      free_vars ~name_of_entity ~bound tuple_get.tuple
  | Core.Expr.If_then_else if_then_else ->
      List.fold_left
        add_unique_name
        (free_vars ~name_of_entity ~bound if_then_else.condition)
        (List.fold_left
          add_unique_name
          (free_vars ~name_of_entity ~bound if_then_else.then_)
          (free_vars ~name_of_entity ~bound if_then_else.else_))
  | Core.Expr.Primitive primitive ->
      List.fold_left
        (fun names expr ->
          List.fold_left add_unique_name names (free_vars ~name_of_entity ~bound expr))
        []
        primitive.arguments

let captures_of_lambda = fun ~name_of_entity ~bound_values (lambda: Core.Expr.lambda) ->
  free_vars ~name_of_entity ~bound:(param_names lambda.params) lambda.body
  |> List.filter (fun name -> has_name bound_values name)

let rec expr_uses_name_as_value = fun ~name_of_entity ~shadowed name expr ->
  match expr with
  | Core.Expr.Constant _ ->
      false
  | Core.Expr.Var var -> (
      match name_of_entity var with
      | Some var_name -> (not (has_name shadowed name)) && String.equal var_name name
      | None -> false
    )
  | Core.Expr.Apply { callee=Core.Expr.Direct callee; arguments } -> (
      match name_of_entity callee with
      | Some callee_name -> (not (String.equal callee_name name))
      && List.exists (expr_uses_name_as_value ~name_of_entity ~shadowed name) arguments
      | None -> List.exists (expr_uses_name_as_value ~name_of_entity ~shadowed name) arguments
    )
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      expr_uses_name_as_value ~name_of_entity ~shadowed name callee
      || List.exists (expr_uses_name_as_value ~name_of_entity ~shadowed name) arguments
  | Core.Expr.Lambda lambda ->
      expr_uses_name_as_value
        ~name_of_entity
        ~shadowed:(param_names lambda.params @ shadowed)
        name
        lambda.body
  | Core.Expr.Let let_ ->
      let binding_names =
        List.map (fun (binding: Core.Expr.binding) -> binding.name) let_.bindings
      in
      let binding_shadowed =
        match let_.rec_flag with
        | Core.Rec_flag.Nonrecursive -> shadowed
        | Core.Rec_flag.Recursive -> binding_names @ shadowed
      in
      List.exists
        (fun (binding: Core.Expr.binding) ->
          expr_uses_name_as_value ~name_of_entity ~shadowed:binding_shadowed name binding.expr)
        let_.bindings
      || expr_uses_name_as_value ~name_of_entity ~shadowed:(binding_names @ shadowed) name let_.body
  | Core.Expr.Sequence sequence ->
      expr_uses_name_as_value ~name_of_entity ~shadowed name sequence.first
      || expr_uses_name_as_value ~name_of_entity ~shadowed name sequence.second
  | Core.Expr.Tuple tuple ->
      List.exists (expr_uses_name_as_value ~name_of_entity ~shadowed name) tuple
  | Core.Expr.Tuple_get tuple_get ->
      expr_uses_name_as_value ~name_of_entity ~shadowed name tuple_get.tuple
  | Core.Expr.If_then_else if_then_else ->
      expr_uses_name_as_value ~name_of_entity ~shadowed name if_then_else.condition
      || expr_uses_name_as_value ~name_of_entity ~shadowed name if_then_else.then_
      || expr_uses_name_as_value ~name_of_entity ~shadowed name if_then_else.else_
  | Core.Expr.Primitive primitive ->
      List.exists (expr_uses_name_as_value ~name_of_entity ~shadowed name) primitive.arguments
