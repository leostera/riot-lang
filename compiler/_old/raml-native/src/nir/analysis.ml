open Std
module HashSet = Collections.HashSet
module Core = Raml_core.Core_ir

type names = {
  seen: string HashSet.t;
  ordered_rev: string list;
}

let empty_names = fun () -> { seen = HashSet.create (); ordered_rev = [] }

let ordered_names = fun names -> List.rev names.ordered_rev

let add_name = fun names name ->
  if HashSet.contains names.seen ~value:name then
    names
  else
    (
      let _ = HashSet.insert names.seen ~value:name in
      { names with ordered_rev = name :: names.ordered_rev }
    )

let add_names = fun names more -> List.fold_left more ~init:names ~fn:add_name

let merge_names = fun left right -> List.fold_left (ordered_names right) ~init:left ~fn:add_name

let bound_of_list = HashSet.from_list

let extend_bound = fun bound names ->
  let bound = HashSet.from_list (HashSet.to_list bound) in
  let () =
    List.for_each names
      ~fn:(fun name ->
        let _ = HashSet.insert bound ~value:name in
        ())
  in
  bound

let bound_has = fun set value -> HashSet.contains set ~value

let param_names = fun params -> List.map ~fn:(fun (param: Core.Expr.param) -> param.name) params

let rec collect_free_vars = fun ~name_of_entity ~bound expr ->
  match expr with
  | Core.Expr.Constant _ ->
      empty_names ()
  | Core.Expr.Var entity_id -> (
      match name_of_entity entity_id with
      | Some name when not (bound_has bound name) -> add_name (empty_names ()) name
      | _ -> empty_names ()
    )
  | Core.Expr.Apply { callee=Core.Expr.Direct _; arguments } ->
      List.fold_left
        arguments
        ~init:(empty_names ())
        ~fn:(fun names argument ->
          merge_names names (collect_free_vars ~name_of_entity ~bound argument))
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      List.fold_left
        arguments
        ~init:(collect_free_vars ~name_of_entity ~bound callee)
        ~fn:(fun names expr -> merge_names names (collect_free_vars ~name_of_entity ~bound expr))
  | Core.Expr.Lambda lambda ->
      collect_free_vars
        ~name_of_entity
        ~bound:(extend_bound bound (param_names lambda.params))
        lambda.body
  | Core.Expr.Let let_ ->
      let binding_names =
        List.map let_.bindings ~fn:(fun (binding: Core.Expr.binding) -> binding.name)
      in
      let binding_scope =
        match let_.rec_flag with
        | Core.Rec_flag.Nonrecursive -> bound
        | Core.Rec_flag.Recursive -> extend_bound bound binding_names
      in
      let binding_free_vars =
        List.fold_left
          let_.bindings
          ~init:(empty_names ())
          ~fn:(fun names (binding: Core.Expr.binding) ->
            merge_names names (collect_free_vars ~name_of_entity ~bound:binding_scope binding.expr))
      in
      merge_names
        binding_free_vars
        (collect_free_vars ~name_of_entity ~bound:(extend_bound bound binding_names) let_.body)
  | Core.Expr.Sequence sequence ->
      merge_names
        (collect_free_vars ~name_of_entity ~bound sequence.first)
        (collect_free_vars ~name_of_entity ~bound sequence.second)
  | Core.Expr.Tuple tuple ->
      List.fold_left
        tuple
        ~init:(empty_names ())
        ~fn:(fun names expr -> merge_names names (collect_free_vars ~name_of_entity ~bound expr))
  | Core.Expr.Tuple_get tuple_get ->
      collect_free_vars ~name_of_entity ~bound tuple_get.tuple
  | Core.Expr.Record record ->
      List.fold_left
        record
        ~init:(empty_names ())
        ~fn:(fun names (field: Core.Expr.record_field) ->
          merge_names names (collect_free_vars ~name_of_entity ~bound field.value))
  | Core.Expr.Record_get record_get ->
      collect_free_vars ~name_of_entity ~bound record_get.record
  | Core.Expr.If_then_else if_then_else ->
      merge_names
        (collect_free_vars ~name_of_entity ~bound if_then_else.condition)
        (merge_names
          (collect_free_vars ~name_of_entity ~bound if_then_else.then_)
          (collect_free_vars ~name_of_entity ~bound if_then_else.else_))
  | Core.Expr.Primitive primitive ->
      List.fold_left
        primitive.arguments
        ~init:(empty_names ())
        ~fn:(fun names expr -> merge_names names (collect_free_vars ~name_of_entity ~bound expr))

let free_vars = fun ~name_of_entity ~bound expr ->
  collect_free_vars ~name_of_entity ~bound:(bound_of_list bound) expr |> ordered_names

let captures_of_lambda = fun ~name_of_entity ~bound_values (lambda: Core.Expr.lambda) ->
  let bound_values = bound_of_list bound_values in
  free_vars ~name_of_entity ~bound:(param_names lambda.params) lambda.body
  |> List.filter ~fn:(fun name -> HashSet.contains bound_values ~value:name)

let rec expr_uses_name_as_value_with_shadowed = fun ~name_of_entity ~shadowed name expr ->
  match expr with
  | Core.Expr.Constant _ ->
      false
  | Core.Expr.Var var -> (
      match name_of_entity var with
      | Some var_name -> (not (HashSet.contains shadowed ~value:name)) && String.equal var_name name
      | None -> false
    )
  | Core.Expr.Apply { callee=Core.Expr.Direct callee; arguments } -> (
      match name_of_entity callee with
      | Some callee_name -> (not (String.equal callee_name name))
      && List.exists (expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name) arguments
      | None -> List.exists (expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name) arguments
    )
  | Core.Expr.Apply { callee=Core.Expr.Indirect callee; arguments } ->
      expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name callee
      || List.exists (expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name) arguments
  | Core.Expr.Lambda lambda ->
      expr_uses_name_as_value_with_shadowed
        ~name_of_entity
        ~shadowed:(extend_bound shadowed (param_names lambda.params))
        name
        lambda.body
  | Core.Expr.Let let_ ->
      let binding_names =
        List.map let_.bindings ~fn:(fun (binding: Core.Expr.binding) -> binding.name)
      in
      let binding_shadowed =
        match let_.rec_flag with
        | Core.Rec_flag.Nonrecursive -> shadowed
        | Core.Rec_flag.Recursive -> extend_bound shadowed binding_names
      in
      List.exists
        (fun (binding: Core.Expr.binding) ->
          expr_uses_name_as_value_with_shadowed
            ~name_of_entity
            ~shadowed:binding_shadowed
            name
            binding.expr)
        let_.bindings
      || expr_uses_name_as_value_with_shadowed
        ~name_of_entity
        ~shadowed:(extend_bound shadowed binding_names)
        name
        let_.body
  | Core.Expr.Sequence sequence ->
      expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name sequence.first
      || expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name sequence.second
  | Core.Expr.Tuple tuple ->
      List.exists (expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name) tuple
  | Core.Expr.Tuple_get tuple_get ->
      expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name tuple_get.tuple
  | Core.Expr.Record record ->
      List.exists
        (fun (field: Core.Expr.record_field) ->
          expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name field.value)
        record
  | Core.Expr.Record_get record_get ->
      expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name record_get.record
  | Core.Expr.If_then_else if_then_else ->
      expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name if_then_else.condition
      || expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name if_then_else.then_
      || expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name if_then_else.else_
  | Core.Expr.Primitive primitive ->
      List.exists (expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed name) primitive.arguments

let expr_uses_name_as_value = fun ~name_of_entity ~shadowed name expr ->
  expr_uses_name_as_value_with_shadowed ~name_of_entity ~shadowed:(bound_of_list shadowed) name expr
