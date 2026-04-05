open Std
module Typ_diagnostic = Diagnostic

type t = {
  exports: Check_result.env;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Typ_diagnostic.t list;
}

type state = {
  file: SemanticTree.file;
  config: TypConfig.t;
  mutable next_type_var_id: int;
  mutable next_hole_id: int;
  mutable diagnostics: Typ_diagnostic.t list;
  mutable expr_traces: Check_result.expr_trace list;
  mutable item_traces: Check_result.item_trace list;
}

let empty_span = Syn.Ceibo.Span.make ~start:0 ~end_:0

let make_state = fun ~config file ->
  {
    file;
    config;
    next_type_var_id = 0;
    next_hole_id = 0;
    diagnostics = [];
    expr_traces = [];
    item_traces = [];
  }

let unique_env = fun env ->
  let rec loop seen acc = function
    | [] -> List.rev acc
    | (name, scheme) :: rest ->
        if List.mem name seen then
          loop seen acc rest
        else
          loop (name :: seen) ((name, scheme) :: acc) rest
  in
  loop [] [] env

let render_env = fun env ->
  env |> unique_env |> List.sort
    (fun (left, _) (right, _) ->
      String.compare left right)

let env_lookup = fun env name ->
  List.find_opt
    (fun (candidate, _) ->
      String.equal candidate name)
    env

let env_names = fun env -> render_env env |> List.map fst

let introduced_names = fun before after ->
  let before_names = env_names before in
  env_names after |> List.filter (fun name -> not (List.mem name before_names))

let env_free_vars = fun env ->
  env |> unique_env |> List.fold_left
    (fun acc (_, scheme) ->
      TypeRepr.union acc (TypeScheme.free_vars scheme))
    []

let fresh_var = fun (state: state) ->
  let id = state.next_type_var_id in
  let () =
    state.next_type_var_id <- state.next_type_var_id + 1
  in
  TypeRepr.Var { id; link = None }

let fresh_hole = fun (state: state) ->
  let hole_id = state.next_hole_id in
  let () =
    state.next_hole_id <- state.next_hole_id + 1
  in
  TypeRepr.Hole hole_id

let instantiate = fun (state: state) (TypeScheme.Forall (quantified, body)) ->
  let mapping = quantified |> List.map (fun quantified_id -> (quantified_id, fresh_var state)) in
  let rec loop ty =
    match TypeRepr.prune ty with
    | TypeRepr.Int ->
        TypeRepr.Int
    | TypeRepr.Float ->
        TypeRepr.Float
    | TypeRepr.Bool ->
        TypeRepr.Bool
    | TypeRepr.String ->
        TypeRepr.String
    | TypeRepr.Char ->
        TypeRepr.Char
    | TypeRepr.Unit ->
        TypeRepr.Unit
    | TypeRepr.Option element ->
        TypeRepr.Option (loop element)
    | TypeRepr.Result (ok_ty, error_ty) ->
        TypeRepr.Result (loop ok_ty, loop error_ty)
    | TypeRepr.Array element ->
        TypeRepr.Array (loop element)
    | TypeRepr.List element ->
        TypeRepr.List (loop element)
    | TypeRepr.Seq element ->
        TypeRepr.Seq (loop element)
    | TypeRepr.Named { name; arguments } ->
        TypeRepr.Named { name; arguments = List.map loop arguments }
    | TypeRepr.Hole hole_id ->
        TypeRepr.Hole hole_id
    | TypeRepr.Tuple members ->
        TypeRepr.Tuple (List.map loop members)
    | TypeRepr.Arrow { label; lhs; rhs } ->
        TypeRepr.Arrow { label; lhs = loop lhs; rhs = loop rhs }
    | TypeRepr.Var var -> (
        match List.assoc_opt var.id mapping with
        | Some replacement -> replacement
        | None -> TypeRepr.Var var
      )
  in
  loop body

let labels_match = fun left right ->
  match (left, right) with
  | (TypeRepr.Nolabel, TypeRepr.Nolabel) -> true
  | (TypeRepr.Labelled left, TypeRepr.Labelled right) -> String.equal left right
  | (TypeRepr.Optional left, TypeRepr.Optional right) -> String.equal left right
  | _ -> false

let type_label_of_body_label = function
  | BodyArena.Positional -> TypeRepr.Nolabel
  | BodyArena.Labeled label -> TypeRepr.Labelled label
  | BodyArena.Optional label -> TypeRepr.Optional label

let generalize = fun env ty ->
  let env_free = env_free_vars env in
  let ty_free = TypeRepr.free_vars ty in
  TypeScheme.Forall (TypeRepr.diff ty_free env_free |> List.rev, ty)

let origin_of_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_pattern = fun (state: state) pat_id ->
  match SemanticTree.find_pattern state.file pat_id with
  | Some node -> SemanticTree.find_origin state.file node.origin_id
  | None -> None

let origin_of_binding = fun (state: state) (binding: BodyArena.binding) ->
  SemanticTree.find_origin state.file binding.origin_id

let diagnostic_span = fun origin ->
  match origin with
  | Some (origin: OriginMap.origin) -> origin.span
  | None -> empty_span

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

exception Unify_error of Typ_diagnostic.mismatch

let rec unify = fun (state: state) ~origin left right ->
  let left = TypeRepr.prune left in
  let right = TypeRepr.prune right in
  match (left, right) with
  | (TypeRepr.Int, TypeRepr.Int)
  | (TypeRepr.Float, TypeRepr.Float)
  | (TypeRepr.Bool, TypeRepr.Bool)
  | (TypeRepr.String, TypeRepr.String)
  | (TypeRepr.Char, TypeRepr.Char)
  | (TypeRepr.Unit, TypeRepr.Unit) ->
      ()
  | TypeRepr.Option left_element, TypeRepr.Option right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error) ->
      let () = unify state ~origin left_ok right_ok in
      unify state ~origin left_error right_error
  | (TypeRepr.Hole _, _)
  | (_, TypeRepr.Hole _) ->
      ()
  | TypeRepr.Array left_element, TypeRepr.Array right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.List left_element, TypeRepr.List right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Seq left_element, TypeRepr.Seq right_element ->
      unify state ~origin left_element right_element
  | TypeRepr.Named { name = left_name; arguments = left_arguments },
    TypeRepr.Named { name = right_name; arguments = right_arguments } ->
      if not (String.equal left_name right_name) then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }))
      else if List.length left_arguments != List.length right_arguments then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }))
      else
        List.iter2 (unify state ~origin) left_arguments right_arguments
  | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
      if List.length left_members != List.length right_members then
        raise
          (Unify_error (Typ_diagnostic.TupleArityMismatch {
            left = TypePrinter.type_to_string left;
            right = TypePrinter.type_to_string right;
            left_arity = List.length left_members;
            right_arity = List.length right_members
          }));
      List.iter2 (unify state ~origin) left_members right_members
  | TypeRepr.Arrow { label = left_label; lhs = left_arg; rhs = left_res },
    TypeRepr.Arrow { label = right_label; lhs = right_arg; rhs = right_res } ->
      if not (labels_match left_label right_label) then
        raise
          (Unify_error (Typ_diagnostic.ExpectedActual {
            expected = TypePrinter.type_to_string left;
            actual = TypePrinter.type_to_string right
          }));
      let () = unify state ~origin left_arg right_arg in
      unify state ~origin left_res right_res
  | TypeRepr.Var left_var, TypeRepr.Var right_var when left_var.id = right_var.id ->
      ()
  | (TypeRepr.Var var, ty)
  | (ty, TypeRepr.Var var) ->
      if TypeRepr.occurs var.id ty then
        raise
          (Unify_error (Typ_diagnostic.OccursCheckFailed {
            variable_id = var.id;
            in_type = TypePrinter.type_to_string ty
          }))
      else
        var.link <- Some ty
  | _ ->
      raise
        (Unify_error (Typ_diagnostic.ExpectedActual {
          expected = TypePrinter.type_to_string left;
          actual = TypePrinter.type_to_string right
        }))

let try_unify = fun (state: state) ~origin left right ->
  try
    unify state ~origin left right;
    ()
  with
  | Unify_error mismatch -> add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch { mismatch_span = diagnostic_span origin; mismatch })

let bind_env = fun env bindings -> bindings @ env

let has_prefix = fun ~prefix text ->
  let prefix_length = String.length prefix in
  if String.length text < prefix_length then
    false
  else
    String.sub text 0 prefix_length = prefix

let aliases_for_local_open = fun env module_path ->
  let prefix = module_path ^ "." in
  env |> List.filter_map (fun (name, scheme) ->
    if has_prefix ~prefix name then
      let suffix =
        String.sub name (String.length prefix) (String.length name - String.length prefix)
      in
      Some (suffix, scheme)
    else
      None)

let env_with_local_open = fun env module_path ->
  let aliases = aliases_for_local_open env module_path in
  bind_env env aliases

let rec constructor_pattern_argument_types = fun (state: state) constructor_ty arguments origin ->
  match arguments with
  | [] -> ([], constructor_ty)
  | _ :: rest ->
      let argument_ty = fresh_var state in
      let result_ty = fresh_var state in
      let () = try_unify
        state
        ~origin
        constructor_ty
        (TypeRepr.Arrow { label = TypeRepr.Nolabel; lhs = argument_ty; rhs = result_ty })
      in
      let (rest_argument_types, final_result_ty) =
        constructor_pattern_argument_types state result_ty rest origin
      in
      (argument_ty :: rest_argument_types, final_result_ty)

let rec bind_pattern = fun (state: state) env pat_id expected_ty ->
  match SemanticTree.find_pattern state.file pat_id with
  | None -> []
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name ->
          [ (name, TypeScheme.Forall ([], expected_ty)) ]
      | BodyArena.PWildcard ->
          []
      | BodyArena.PInt _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Int in
          []
      | BodyArena.PFloat _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Float in
          []
      | BodyArena.PBool _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Bool in
          []
      | BodyArena.PString _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.String in
          []
      | BodyArena.PChar _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Char in
          []
      | BodyArena.PUnit ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.Unit in
          []
      | BodyArena.PTuple elements ->
          let element_types =
            List.map (fun _ -> fresh_var state) elements
          in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (TypeRepr.Tuple element_types) in
          List.map2 (bind_pattern state env) elements element_types |> List.flatten
      | BodyArena.PConstructor { constructor; arguments } -> (
          match env_lookup env constructor with
          | Some (_, scheme) ->
              let origin = origin_of_pattern state pat_id in
              let constructor_ty = instantiate state scheme in
              let (argument_types, result_ty) =
                constructor_pattern_argument_types state constructor_ty arguments origin
              in
              let () = try_unify state ~origin expected_ty result_ty in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
          | None ->
              let argument_types = List.map (fun _ -> fresh_var state) arguments in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
        )
      | BodyArena.PList elements ->
          let element_ty = fresh_var state in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (TypeRepr.List element_ty)
          in
          elements
          |> List.map (fun element_id -> bind_pattern state env element_id element_ty)
          |> List.flatten
      | BodyArena.PAlias { pattern_id; alias } ->
          let bindings = bind_pattern state env pattern_id expected_ty in
          (alias, TypeScheme.Forall ([], expected_ty)) :: bindings
      | BodyArena.PPolyVariant { payload; _ } -> (
          match payload with
          | Some payload_id -> bind_pattern state env payload_id (fresh_hole state)
          | None -> []
        )
      | BodyArena.PUnsupported _ ->
          []
    )

let record_expr_trace = fun (state: state) expr_id origin_id env_before inferred_type ->
  state.expr_traces <- (
    { Check_result.expr_id; origin_id; env_before = render_env env_before; inferred_type }:
      Check_result.expr_trace
  )
  :: state.expr_traces

let rec infer_expr = fun (state: state) env expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> fresh_hole state
  | Some expr ->
      let inferred_type =
        match expr.desc with
        | BodyArena.EVar name -> (
            match env_lookup env name with
            | Some (_, scheme) -> instantiate state scheme
            | None ->
                let hole = fresh_hole state in
                let () = add_diagnostic
                  state
                  (Typ_diagnostic.UnboundName {
                    reference_span = diagnostic_span (origin_of_expr state expr_id);
                    name
                  }) in
                hole
          )
        | BodyArena.EInt _ ->
            TypeRepr.Int
        | BodyArena.EFloat _ ->
            TypeRepr.Float
        | BodyArena.EBool _ ->
            TypeRepr.Bool
        | BodyArena.EString _ ->
            TypeRepr.String
        | BodyArena.EChar _ ->
            TypeRepr.Char
        | BodyArena.EUnit ->
            TypeRepr.Unit
        | BodyArena.ETuple elements ->
            TypeRepr.Tuple (List.map (infer_expr state env) elements)
        | BodyArena.EArray elements ->
            let element_ty = fresh_var state in
            let () =
              List.iter
                (fun element_id ->
                  let inferred_element = infer_expr state env element_id in
                  try_unify state ~origin:(origin_of_expr state element_id) element_ty inferred_element)
                elements
            in
            TypeRepr.Array element_ty
        | BodyArena.ESequence elements -> (
            match List.rev elements with
            | [] -> TypeRepr.Unit
            | last_id :: reversed_prefix ->
                let prefix = List.rev reversed_prefix in
                let () =
                  List.iter
                    (fun element_id ->
                      let element_ty = infer_expr state env element_id in
                      try_unify state ~origin:(origin_of_expr state element_id) element_ty TypeRepr.Unit)
                    prefix
                in
                infer_expr state env last_id
          )
        | BodyArena.EFun (parameters, body_id) ->
            let rec lower_parameters env = function
              | [] ->
                  infer_expr state env body_id
              | (parameter: BodyArena.function_parameter) :: rest ->
                  let arg_ty = fresh_var state in
                  let bindings = bind_pattern state env parameter.pattern_id arg_ty in
                  let body_ty = lower_parameters (bind_env env bindings) rest in
                  TypeRepr.Arrow {
                    label = type_label_of_body_label parameter.label;
                    lhs = arg_ty;
                    rhs = body_ty
                  }
            in
            lower_parameters env parameters
        | BodyArena.EApply (callee_id, arguments) ->
            let callee_ty = infer_expr state env callee_id in
            let rec apply current_ty = function
              | [] -> current_ty
              | (argument: BodyArena.apply_argument) :: rest ->
                  let argument_ty = infer_expr state env argument.value_id in
                  let result_ty = fresh_var state in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    current_ty
                    (TypeRepr.Arrow {
                      label = type_label_of_body_label argument.label;
                      lhs = argument_ty;
                      rhs = result_ty
                    }) in
                  apply result_ty rest
            in
            apply callee_ty arguments
        | BodyArena.EIndex (collection_id, index_id) ->
            let collection_ty = infer_expr state env collection_id in
            let index_ty = infer_expr state env index_id in
            let element_ty = fresh_var state in
            let () = try_unify
              state
              ~origin:(origin_of_expr state index_id)
              index_ty
              TypeRepr.Int in
            begin match TypeRepr.prune collection_ty with
            | TypeRepr.String ->
                TypeRepr.Char
            | _ ->
                let () = try_unify
                  state
                  ~origin:(origin_of_expr state collection_id)
                  collection_ty
                  (TypeRepr.Array element_ty)
                in
                element_ty
            end
        | BodyArena.ELet (binding_ids, body_id) ->
            let env = infer_binding_group state env binding_ids in
            infer_expr state env body_id
        | BodyArena.EIf (condition_id, then_id, else_id) ->
            let condition_ty = infer_expr state env condition_id in
            let () = try_unify
              state
              ~origin:(origin_of_expr state condition_id)
              condition_ty
              TypeRepr.Bool in
            let then_ty = infer_expr state env then_id in
            let else_ty = infer_expr state env else_id in
            let () = try_unify state ~origin:(origin_of_expr state expr_id) then_ty else_ty in
            then_ty
        | BodyArena.EMatch (scrutinee_id, cases) ->
            let scrutinee_ty = infer_expr state env scrutinee_id in
            let result_ty = fresh_var state in
            let () =
              List.iter
                (fun (case: BodyArena.match_case) ->
                  let bindings = bind_pattern state env case.pattern_id scrutinee_ty in
                  let case_ty = infer_expr state (bind_env env bindings) case.body_id in
                  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty)
                cases
            in
            result_ty
        | BodyArena.ETry (body_id, cases) ->
            let body_ty = infer_expr state env body_id in
            let exn_ty = TypeRepr.Named { name = "exn"; arguments = [] } in
            let result_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state body_id) result_ty body_ty in
            let () =
              List.iter
                (fun (case: BodyArena.match_case) ->
                  let bindings = bind_pattern state env case.pattern_id exn_ty in
                  let case_ty = infer_expr state (bind_env env bindings) case.body_id in
                  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty)
                cases
            in
            result_ty
        | BodyArena.EPolyVariant { payload; _ } ->
            let () =
              match payload with
              | Some payload_id ->
                  let _ = infer_expr state env payload_id in
                  ()
              | None -> ()
            in
            fresh_hole state
        | BodyArena.ELocalOpen { module_path; body_id } ->
            infer_expr state (env_with_local_open env module_path) body_id
        | BodyArena.EUnsupported summary ->
            let hole = fresh_hole state in
            let () = add_diagnostic
              state
              (Typ_diagnostic.UnsupportedSemanticExpression {
                expression_span = diagnostic_span (origin_of_expr state expr_id);
                summary
              }) in
            hole
        | BodyArena.EHole _ ->
            fresh_hole state
      in
      let () = record_expr_trace state expr_id expr.origin_id env inferred_type in
      inferred_type

and infer_binding_group = fun (state: state) env binding_ids ->
  let bindings = binding_ids |> List.filter_map (SemanticTree.find_binding state.file) in
  let recursive =
    match bindings with
    | [] -> false
    | (binding: BodyArena.binding) :: _ -> binding.recursive
  in
  if recursive then
    infer_recursive_group state env bindings
  else
    infer_nonrecursive_group state env bindings

and infer_nonrecursive_group = fun (state: state) env bindings ->
  let inferred_bindings =
    List.map
      (fun (binding: BodyArena.binding) ->
        let value_ty = infer_expr state env binding.value_id in
        let bound_entries = bind_pattern state env binding.pattern_id value_ty in
        let generalized = bound_entries
        |> List.map (fun (name, TypeScheme.Forall (_, ty)) -> (name, generalize env ty)) in
        (binding, generalized))
      bindings
  in
  List.fold_left (fun env (_, entries) -> bind_env env entries) env inferred_bindings

and infer_recursive_group = fun (state: state) env bindings ->
  let names = bindings |> List.map (fun (binding: BodyArena.binding) -> (binding, binding.name)) in
  if List.exists (fun (_, name) -> Option.is_none name) names then
    (
      let () =
        List.iter
          (fun ((binding: BodyArena.binding), _) ->
            add_diagnostic
              state
              (Typ_diagnostic.RecursiveGroupRequiresSimpleVariableBinders {
                binding_span = diagnostic_span (origin_of_binding state binding)
              }))
          names
      in
      infer_nonrecursive_group state env bindings
    )
  else
    let placeholders =
      names
      |> List.filter_map
        (fun ((binding: BodyArena.binding), name) ->
          match name with
          | Some name -> Some (binding, name, fresh_var state)
          | None -> None)
    in
    let provisional_env = placeholders
    |> List.map (fun (_, name, ty) -> (name, TypeScheme.Forall ([], ty)))
    |> bind_env env in
    let () =
      List.iter
        (fun ((binding: BodyArena.binding), _, placeholder_ty) ->
          let value_ty = infer_expr state provisional_env binding.value_id in
          try_unify state ~origin:(origin_of_binding state binding) placeholder_ty value_ty)
        placeholders
    in
    let generalized = placeholders |> List.map (fun (_, name, ty) -> (name, generalize env ty)) in
    bind_env env generalized

let prelude_names = fun (config: TypConfig.t) -> config.prelude |> List.map fst

let ambient_names = fun (config: TypConfig.t) -> config.ambient |> List.map fst

let export_env = fun config env ->
  let hidden_names = prelude_names config @ ambient_names config in
  render_env env |> List.filter (fun (name, _) -> not (List.mem name hidden_names))

let introduced_entries = fun before after ->
  let introduced = introduced_names before after in
  render_env after |> List.filter (fun (name, _) -> List.mem name introduced)

let qualify_name = fun scope_path name ->
  match scope_path with
  | [] -> name
  | _ -> String.concat "." scope_path ^ "." ^ name

let qualify_entries = fun scope_path entries ->
  List.map (fun (name, scheme) -> (qualify_name scope_path name, scheme)) entries

let scope_key = fun scope_path -> String.concat "." scope_path

let scope_prefix_keys = fun scope_path ->
  let rec loop acc current = function
    | [] -> List.rev acc
    | segment :: rest ->
        let current = current @ [ segment ] in
        loop (scope_key current :: acc) current rest
  in
  loop [ scope_key [] ] [] scope_path

let scope_locals_for = fun scope_entries scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_entries with
      | Some entries -> bind_env acc entries
      | None -> acc)
    []

let update_scope_entries = fun scope_entries scope_path entries ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_entries with
    | Some entries -> entries
    | None -> []
  in
  let updated = bind_env existing entries in
  (key, updated) :: List.remove_assoc key scope_entries

let scope_opens_for = fun scope_opens scope_path ->
  scope_prefix_keys scope_path |> List.fold_left
    (fun acc key ->
      match List.assoc_opt key scope_opens with
      | Some modules -> acc @ modules
      | None -> acc)
    []

let update_scope_opens = fun scope_opens scope_path module_path ->
  let key = scope_key scope_path in
  let existing =
    match List.assoc_opt key scope_opens with
    | Some modules -> modules
    | None -> []
  in
  let updated = existing @ [ module_path ] in
  (key, updated) :: List.remove_assoc key scope_opens

let env_for_item_scope = fun export_env scope_entries scope_opens scope_path ->
  let locals = scope_locals_for scope_entries scope_path in
  let base_env = bind_env export_env locals in
  scope_opens_for scope_opens scope_path |> List.fold_left env_with_local_open base_env

let infer_file = fun ~config file ->
  let state = make_state ~config file in
  let initial_env = bind_env config.prelude config.ambient in
  let rec loop export_state scope_entries scope_opens = function
    | [] -> export_state
    | item :: rest -> (
        match item with
        | ItemTree.Type type_item ->
            let visible_exports_before = export_env config export_state in
            let introduced = TypeDecl.constructor_entries type_item.declaration in
            let (export_state, scope_entries) =
              match type_item.scope_path with
              | [] ->
                  (bind_env export_state introduced, scope_entries)
              | scope_path ->
                  (
                    bind_env export_state (qualify_entries scope_path introduced),
                    update_scope_entries scope_entries scope_path introduced
                  )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = type_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state scope_entries scope_opens rest
        | ItemTree.Exception exception_item ->
            let visible_exports_before = export_env config export_state in
            let introduced = [ (exception_item.exception_name, exception_item.scheme) ] in
            let (export_state, scope_entries) =
              match exception_item.scope_path with
              | [] ->
                  (bind_env export_state introduced, scope_entries)
              | scope_path ->
                  (
                    bind_env export_state (qualify_entries scope_path introduced),
                    update_scope_entries scope_entries scope_path introduced
                  )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = exception_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state scope_entries scope_opens rest
        | ItemTree.Value value_item ->
            let visible_exports_before = export_env config export_state in
            let item_env = env_for_item_scope export_state scope_entries scope_opens value_item.scope_path in
            let env_after_item = infer_binding_group state item_env value_item.binding_ids in
            let introduced = introduced_entries item_env env_after_item in
            let (export_state, scope_entries) =
              match value_item.scope_path with
              | [] ->
                  (env_after_item, scope_entries)
              | scope_path ->
                  (
                    bind_env export_state (qualify_entries scope_path introduced),
                    update_scope_entries scope_entries scope_path introduced
                  )
            in
            let exports_after = export_env config export_state in
            let binding_names = introduced_names visible_exports_before exports_after in
            let () =
              state.item_traces <- (
                { Check_result.item_id = value_item.item_id; binding_names; exports_after }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state scope_entries scope_opens rest
        | ItemTree.Open open_item ->
            let scope_opens = update_scope_opens scope_opens open_item.scope_path open_item.module_path in
            let exports_after = export_env config export_state in
            let () =
              state.item_traces <- (
                {
                  Check_result.item_id = open_item.item_id;
                  binding_names = [];
                  exports_after
                }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state scope_entries scope_opens rest
        | ItemTree.Unsupported unsupported_item ->
            let exports_after = export_env config export_state in
            let () =
              state.item_traces <- (
                {
                  Check_result.item_id = unsupported_item.item_id;
                  binding_names = [];
                  exports_after
                }:
                  Check_result.item_trace
              )
              :: state.item_traces
            in
            loop export_state scope_entries scope_opens rest)
  in
  let exports = loop initial_env [] [] (ItemTree.items file.item_tree)
  in
  {
    exports = export_env config exports;
    item_traces = List.rev state.item_traces;
    expr_traces = List.rev state.expr_traces;
    diagnostics = List.rev state.diagnostics
  }
