open Std
open Analysis
open Diagnostics
open Model
module Typ_diagnostic = Diagnostic
module Region = Region
open State

(* Use Super since open Std brings an Env module of its own that shadows ours *)

module Env = Super.Env
open Env

type t = {
  exports: Check_result.env;
  type_decls: FileSummary.type_decl list;
  item_traces: Check_result.item_trace list;
  expr_traces: Check_result.expr_trace list;
  diagnostics: Typ_diagnostic.t list;
}

type record_type_decl = Env.Label_env.record_decl

type named_owner = {
  type_constructor_id: TypeConstructorId.t;
}

type variance = TypeDecl.variance

type state = State.t

module Label_name_map = Collections.Map.Make (String)

let empty_span = Syn.Ceibo.Span.make ~start:0 ~end_:0

let missing_record_decl_param_hole_id = (-200)

let qualify_name = State.qualify_name

let bind_type_decls = State.bind_type_decls

let type_decls_for_include = State.type_decls_for_include

let type_decls_for_module_alias = State.type_decls_for_module_alias

let make_state = State.make

let fresh_var = State.fresh_var

let fresh_binding_ident = fun state name ->
  Binding.make_ident ~local_id:(State.fresh_binding_local_id state) ~name

let fresh_hole = State.fresh_hole

let make_type = State.make_type

let set_visible_type_decls = State.set_visible_type_decls

let with_local_level_gen = State.with_local_level_gen

let prelude_env = fun (state: state) (config: TypConfig.t) ->
  Env.bind
    (Env.of_entries ~make_ident:(fresh_binding_ident state) ~provenance:Binding.Prelude config.prelude)
    (Env.of_type_decls LanguagePrelude.type_decls)

let ambient_env = fun (state: state) (config: TypConfig.t) ->
  Env.of_entries ~make_ident:(fresh_binding_ident state) ~provenance:Binding.Ambient config.ambient

let ambient_type_env = fun (_state: state) (config: TypConfig.t) -> Env.of_type_decls config.ambient_type_decls

let view = fun ty -> TypeRepr.view (TypeRepr.prune ty)

let pattern_binding = fun (state: state) pat_id ~name ~scheme ->
  Binding.make
    ~ident:(fresh_binding_ident state name)
    ~path:(IdentPath.of_name name)
    ~scheme
    ~provenance:(Binding.Lowered_pattern pat_id)

let generalized_pattern_binding = fun (state: state) pat_id ~name ty ->
  pattern_binding state pat_id ~name ~scheme:(TypeScheme.of_type ty)

let type_item_env = fun (_state: state) (type_item: ItemTree.type_item) ->
  Env.of_type_decls
    [ { FileSummary.scope_path = IdentPath.empty; declaration = type_item.declaration } ]

let exception_bindings = fun (state: state) (exception_item: ItemTree.exception_item) ->
  Env.singleton
    ~make_ident:(fresh_binding_ident state)
    ~name:exception_item.exception_name
    ~scheme:exception_item.scheme
    ~provenance:(Binding.Exception {
      name = exception_item.exception_name;
      scope_path = exception_item.scope_path
    })

let declared_value_bindings = fun (state: state) (declared_value_item: ItemTree.declared_value_item) ->
  Env.singleton
    ~make_ident:(fresh_binding_ident state)
    ~name:declared_value_item.value_name
    ~scheme:declared_value_item.scheme
    ~provenance:(Binding.Declared_value {
      name = declared_value_item.value_name;
      scope_path = declared_value_item.scope_path
    })

let instantiate = fun (state: state) scheme ->
  TypeScheme.instantiate
    ~fresh_var:(fun () -> fresh_var state)
    ~make:(make_type state)
    ~next_mark:(fun () -> Region.next_mark state.regions)
    scheme
  |> State.resolve_type state

let substitute_type_vars = fun (state: state) ty mapping ->
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int ->
        ty
    | TypeRepr.Float ->
        ty
    | TypeRepr.Bool ->
        ty
    | TypeRepr.String ->
        ty
    | TypeRepr.Char ->
        ty
    | TypeRepr.Unit ->
        ty
    | TypeRepr.Option element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make_type state (TypeRepr.Option element')
    | TypeRepr.Result (ok_ty, error_ty) ->
        let ok_ty' = loop ok_ty in
        let error_ty' = loop error_ty in
        if Std.Ptr.equal ok_ty ok_ty' && Std.Ptr.equal error_ty error_ty' then
          ty
        else
          make_type state (TypeRepr.Result (ok_ty', error_ty'))
    | TypeRepr.Array element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make_type state (TypeRepr.Array element')
    | TypeRepr.List element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make_type state (TypeRepr.List element')
    | TypeRepr.Seq element ->
        let element' = loop element in
        if Std.Ptr.equal element element' then
          ty
        else
          make_type state (TypeRepr.Seq element')
    | TypeRepr.Named { type_constructor; name; arguments } ->
        let arguments' = List.map loop arguments in
        if List.for_all2 Std.Ptr.equal arguments arguments' then
          ty
        else
          make_type state (TypeRepr.Named { type_constructor; name; arguments = arguments' })
    | TypeRepr.Hole hole_id ->
        make_type state (TypeRepr.Hole hole_id)
    | TypeRepr.Tuple members ->
        let members' = List.map loop members in
        if List.for_all2 Std.Ptr.equal members members' then
          ty
        else
          make_type state (TypeRepr.Tuple members')
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let lhs' = loop lhs in
        let rhs' = loop rhs in
        if Std.Ptr.equal lhs lhs' && Std.Ptr.equal rhs rhs' then
          ty
        else
          make_type state (TypeRepr.Arrow { label; lhs = lhs'; rhs = rhs' })
    | TypeRepr.Var var -> (
        match var.link with
        | Some linked -> loop linked
        | None ->
            match Collections.HashMap.get mapping var.id with
            | Some replacement -> replacement
            | None -> ty
      )
  in
  loop ty

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

let diagnostic_label_of_type_label = function
  | TypeRepr.Nolabel -> Typ_diagnostic.PositionalArgument
  | TypeRepr.Labelled label -> Typ_diagnostic.LabeledArgument label
  | TypeRepr.Optional label -> Typ_diagnostic.OptionalArgument label

let diagnostic_label_of_body_label = function
  | BodyArena.Positional -> Typ_diagnostic.PositionalArgument
  | BodyArena.Labeled label -> Typ_diagnostic.LabeledArgument label
  | BodyArena.Optional label -> Typ_diagnostic.OptionalArgument label

let argument_matches_parameter_label = fun parameter_label argument_label ->
  match (parameter_label, argument_label) with
  | (TypeRepr.Nolabel, BodyArena.Positional) -> true
  | (TypeRepr.Labelled expected, BodyArena.Labeled actual) -> String.equal expected actual
  | (TypeRepr.Optional expected, BodyArena.Optional actual)
  | (TypeRepr.Optional expected, BodyArena.Labeled actual) -> String.equal expected actual
  | _ -> false

let take_matching_argument = fun parameter_label arguments ->
  let rec loop prefix = function
    | [] -> None
    | ((argument: BodyArena.apply_argument) as candidate) :: rest ->
        if argument_matches_parameter_label parameter_label argument.label then
          Some (candidate, List.rev_append prefix rest)
        else
          loop (candidate :: prefix) rest
  in
  loop [] arguments

let visible_type_decl = fun (state: state) name ->
  Collections.HashMap.get state.visible_type_decl_by_path name

let visible_type_decl_by_id = fun (state: state) type_constructor_id ->
  Collections.HashMap.get state.visible_type_decl_by_id type_constructor_id

let resolve_type_constructor_id = fun (state: state) type_constructor name ->
  match State.resolve_named_type_constructor state type_constructor name with
  | TypeRepr.Resolved type_constructor_id -> Some type_constructor_id
  | TypeRepr.Unresolved -> None

let owner_of_type = fun (state: state) ty ->
  match view (State.resolve_type state ty) with
  | TypeRepr.Named { type_constructor=TypeRepr.Resolved type_constructor_id; _ } -> Some {
    type_constructor_id
  }
  | TypeRepr.Named _ -> None
  | _ -> None

let rec result_type_of_type = fun ty ->
  let ty = TypeRepr.prune ty in
  match TypeRepr.view ty with
  | TypeRepr.Arrow { rhs; _ } -> result_type_of_type rhs
  | _ -> ty

let resolve_constructor_entry = fun (state: state) env constructor ~expected_ty ->
  let candidates = Env.lookup_constructors env constructor in
  match owner_of_type state expected_ty with
  | Some expected_owner -> (
      match Env.lookup_owned_constructor env constructor expected_owner.type_constructor_id with
      | Some candidate -> Some candidate
      | None -> (
          match
            List.filter
              (fun candidate ->
                TypeConstructorId.equal
                  expected_owner.type_constructor_id
                  (Env.Constructor_env.owner_type_constructor_id candidate))
              candidates
          with
          | candidate :: _ -> Some candidate
          | [] -> (
              match candidates with
              | candidate :: _ -> Some candidate
              | [] -> None
            )
        )
    )
  | None -> (
      match candidates with
      | candidate :: _ -> Some candidate
      | [] -> None
    )

let resolve_constructor_without_expected = fun env constructor ->
  Env.lookup_constructors env constructor |> function
  | candidate :: _ -> Some candidate
  | [] -> None

let record_decl_matches_owner = fun (record_decl: record_type_decl) owner ->
  TypeConstructorId.equal (Env.Label_env.owner_type_constructor_id record_decl) owner.type_constructor_id

let instantiate_record_decl = fun (state: state) (record_decl: record_type_decl) ->
  let mapping = Collections.HashMap.with_capacity 8 in
  let () =
    Env.Label_env.param_ids record_decl
    |> List.iter
      (fun quantified_id ->
        let _ = Collections.HashMap.insert mapping quantified_id (fresh_var state) in
        ())
  in
  let owner_path =
    match Collections.HashMap.get
      state.visible_type_decl_by_id
      (Env.Label_env.owner_type_constructor_id record_decl) with
    | Some type_decl -> qualify_name type_decl.scope_path type_decl.declaration.type_name
    | None -> Env.Label_env.owner_path record_decl
  in
  let owner_ty = make_type state
    (
      TypeRepr.Named {
        type_constructor = TypeRepr.Resolved (Env.Label_env.owner_type_constructor_id record_decl);
        name = owner_path;
        arguments =
          Env.Label_env.param_ids record_decl |> List.map
            (fun quantified_id ->
              match Collections.HashMap.get mapping quantified_id with
              | Some ty -> ty
              | None -> make_type state (TypeRepr.Hole missing_record_decl_param_hole_id));
      }
    )
  in
  let field_types =
    Env.Label_env.labels record_decl
    |> List.fold_left
      (fun acc (field: TypeDecl.label) ->
        Label_name_map.add
          (Env.Label_env.lookup_name field.name)
          (substitute_type_vars state field.field_type mapping)
          acc)
      Label_name_map.empty
  in
  (owner_ty, field_types)

let record_field_type = fun field_types label_name ->
  Label_name_map.find_opt (Env.Label_env.lookup_name label_name) field_types

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let add_record_resolution_error = fun (state: state) ~span ~context reason ->
  add_diagnostic
    state
    (Typ_diagnostic.RecordResolutionError { operation_span = span; context; reason })

let add_or_pattern_bindings_mismatch = fun (state: state) ~span ~expected_names ~actual_names ->
  add_diagnostic
    state
    (Typ_diagnostic.OrPatternBindingsMismatch { pattern_span = span; expected_names; actual_names })

let resolve_record_decl = fun env (state: state) ~field_names ~owner_hint ~span ~context ->
  let name_candidates =
    match field_names with
    | first_field :: remaining_fields ->
        Env.lookup_record_decls env first_field |> List.filter
          (fun (record_decl: record_type_decl) ->
            Env.Label_env.matches_fields record_decl remaining_fields)
    | [] -> Env.record_decls env
  in
  let candidates =
    match owner_hint with
    | Some owner -> (
        match Env.lookup_record_decl_by_owner env owner.type_constructor_id with
        | Some record_decl when Env.Label_env.matches_fields record_decl field_names -> [
          record_decl
        ]
        | _ -> name_candidates
        |> List.filter
          (fun (record_decl: record_type_decl) -> record_decl_matches_owner record_decl owner)
      )
    | None -> name_candidates
  in
  match candidates with
  | [ record_decl ] ->
      Some record_decl
  | [] ->
      let unknown_labels = field_names
      |> List.filter (fun field_name -> List.is_empty (Env.lookup_record_decls env field_name)) in
      let reason =
        if not (List.is_empty unknown_labels) then
          Typ_diagnostic.UnknownRecordLabels unknown_labels
        else
          Typ_diagnostic.IncompatibleRecordLabels field_names
      in
      let () = add_record_resolution_error state ~span ~context reason in
      None
  | _ ->
      let () = add_record_resolution_error
        state
        ~span
        ~context
        (Typ_diagnostic.AmbiguousRecordLabels field_names) in
      None

let constructor_payload_types = fun (constructor: TypeDecl.constructor) ->
  let rec loop acc ty =
    match view ty with
    | TypeRepr.Arrow { lhs; rhs; _ } -> loop (lhs :: acc) rhs
    | _ -> List.rev acc
  in
  loop [] (TypeScheme.body constructor.scheme)

let origin_label_of_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> None
  | Some expr -> (
      match SemanticTree.find_origin state.file expr.origin_id with
      | Some origin -> Some origin.label
      | None -> None
    )

let rec is_nonexpansive_expr = fun (state: state) expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> false
  | Some expr ->
      match expr.desc with
      | BodyArena.EVar _
      | BodyArena.EInt _
      | BodyArena.EFloat _
      | BodyArena.EBool _
      | BodyArena.EString _
      | BodyArena.EChar _
      | BodyArena.EUnit ->
          true
      | BodyArena.ETuple element_ids ->
          List.for_all (is_nonexpansive_expr state) element_ids
      | BodyArena.EFun _ ->
          true
      | BodyArena.EApply (callee_id, arguments) ->
          let arguments_nonexpansive = arguments
          |> List.for_all
            (fun (argument: BodyArena.apply_argument) -> is_nonexpansive_expr state argument.value_id) in
          if not arguments_nonexpansive then
            false
          else
            (
              match origin_label_of_expr state expr_id with
              | Some "constructor_apply_expression"
              | Some "list_literal_apply" ->
                  true
              | Some "infix_expression" -> (
                  match SemanticTree.find_expr state.file callee_id with
                  | Some { desc=BodyArena.EVar name; _ } when IdentPath.equal
                    name
                    (IdentPath.of_name "::") -> true
                  | _ -> false
                )
              | _ ->
                  false
            )
      | BodyArena.ERecord { base_id=None; fields } ->
          fields
          |> List.for_all
            (fun (field: BodyArena.record_expr_field) -> is_nonexpansive_expr state field.value_id)
      | BodyArena.ERecord { base_id=Some _; _ } ->
          false
      | BodyArena.EFieldAccess { receiver_id; _ } ->
          is_nonexpansive_expr state receiver_id
      | BodyArena.EIndex _
      | BodyArena.EArray _
      | BodyArena.ESequence _
      | BodyArena.EUnsupported _
      | BodyArena.EHole _ ->
          false
      | BodyArena.ELet (binding_ids, body_id) ->
          List.for_all (is_nonexpansive_binding state) binding_ids && is_nonexpansive_expr state body_id
      | BodyArena.EIf (condition_id, then_id, else_id) ->
          is_nonexpansive_expr state condition_id
          && is_nonexpansive_expr state then_id
          && is_nonexpansive_expr state else_id
      | BodyArena.EMatch (scrutinee_id, cases) ->
          is_nonexpansive_expr state scrutinee_id && List.for_all
            (fun (case: BodyArena.match_case) ->
              let guard_nonexpansive =
                match case.guard_id with
                | Some guard_id -> is_nonexpansive_expr state guard_id
                | None -> true
              in
              guard_nonexpansive && is_nonexpansive_expr state case.body_id)
            cases
      | BodyArena.ETry _ ->
          false
      | BodyArena.EPolyVariant { payload; _ } -> (
          match payload with
          | Some payload_id -> is_nonexpansive_expr state payload_id
          | None -> true
        )
      | BodyArena.ELocalOpen { body_id; _ } ->
          is_nonexpansive_expr state body_id

and is_nonexpansive_binding = fun (state: state) binding_id ->
  match SemanticTree.find_binding state.file binding_id with
  | Some (binding: BodyArena.binding) -> is_nonexpansive_expr state binding.value_id
  | None -> false

let generalize_binding = fun (_state: state) (_frame: Region.frame) ty -> TypeScheme.of_type ty

let generalize_entry_groups = fun (state: state) (frame: Region.frame) entry_groups ->
  let roots = entry_groups
  |> List.concat_map
    (fun entries -> entries |> List.map (fun entry -> TypeScheme.body (Binding.scheme entry))) in
  let () =
    if not (List.is_empty roots) then
      Region.generalize_reachable_vars state.regions frame roots
  in
  entry_groups |> List.map
    (fun entries ->
      entries |> List.map
        (fun entry ->
          Binding.with_scheme
            (generalize_binding state frame (TypeScheme.body (Binding.scheme entry)))
            entry))

let lower_expansive_binding_vars = fun (state: state) (frame: Region.frame) expr_id ty ->
  if is_nonexpansive_expr state expr_id then
    ()
  else
    let boundary_level = Region.boundary_level frame in
    let generation = Region.mark_roots state.regions [ ty ] in
    let seen = Collections.HashMap.with_capacity 16 in
    let rec lower = function
      | [] -> ()
      | (variance, ty) :: rest ->
          let ty = TypeRepr.prune ty in
          if not (Int.equal ty.mark generation) then
            lower rest
          else
            let order = ty.mark_order in
            let (should_process, variance) =
              match Collections.HashMap.get seen order with
              | Some seen_variance ->
                  let joined = TypeDecl.join_variance seen_variance variance in
                  if joined = seen_variance then
                    (false, seen_variance)
                  else
                    (
                      let _ = Collections.HashMap.insert seen order joined in
                      (true, joined)
                    )
              | None ->
                  let _ = Collections.HashMap.insert seen order variance in
                  (true, variance)
            in
            if not should_process || TypeRepr.level ty <= boundary_level then
              lower rest
            else
              let () =
                match variance with
                | TypeDecl.Covariant -> ()
                | TypeDecl.Contravariant
                | TypeDecl.Invariant ->
                    if TypeRepr.level ty > boundary_level then
                      (
                        TypeRepr.set_level ty boundary_level;
                        Region.add_to_pool state.regions ~level:boundary_level ty |> ignore
                      )
              in
              let rest =
                match TypeRepr.view ty with
                | TypeRepr.Int
                | TypeRepr.Float
                | TypeRepr.Bool
                | TypeRepr.String
                | TypeRepr.Char
                | TypeRepr.Unit
                | TypeRepr.Hole _
                | TypeRepr.Var _ ->
                    rest
                | TypeRepr.Option element
                | TypeRepr.List element
                | TypeRepr.Seq element ->
                    (variance, element) :: rest
                | TypeRepr.Result (ok_ty, error_ty) ->
                    (variance, ok_ty) :: (variance, error_ty) :: rest
                | TypeRepr.Array element ->
                    (TypeDecl.Invariant, element) :: rest
                | TypeRepr.Named { type_constructor; name; arguments } ->
                    let parameter_variances =
                      match resolve_type_constructor_id state type_constructor name with
                      | Some type_constructor_id -> (
                          match visible_type_decl_by_id state type_constructor_id with
                          | Some type_decl -> type_decl.declaration.param_variances
                          | None -> List.map (fun _ -> TypeDecl.Invariant) arguments
                        )
                      | None -> List.map (fun _ -> TypeDecl.Invariant) arguments
                    in
                    let rec add_arguments acc arguments parameter_variances =
                      match (arguments, parameter_variances) with
                      | (argument :: rest_arguments, parameter_variance :: rest_variances) -> add_arguments
                        ((TypeDecl.compose_variance variance parameter_variance, argument) :: acc)
                        rest_arguments
                        rest_variances
                      | _ -> acc
                    in
                    add_arguments rest arguments parameter_variances
                | TypeRepr.Tuple members ->
                    List.fold_left (fun acc member -> (variance, member) :: acc) rest members
                | TypeRepr.Arrow { lhs; rhs; _ } ->
                    (TypeDecl.flip_variance variance, lhs) :: (variance, rhs) :: rest
              in
              lower rest
    in
    let initial = ref [] in
    let () =
      Region.iter_owned_nodes frame
        (fun node ->
          let node = TypeRepr.prune node in
          if Int.equal node.mark generation then
            initial := (TypeDecl.Covariant, node) :: !initial)
    in
    lower !initial

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

exception Unify_error of Typ_diagnostic.mismatch

let unify = fun (state: state) ~origin left right ->
  let named_types_match left_type_constructor left_name right_type_constructor right_name =
    let left_type_constructor = State.resolve_named_type_constructor state left_type_constructor left_name in
    let right_type_constructor = State.resolve_named_type_constructor state right_type_constructor right_name in
    match (left_type_constructor, right_type_constructor) with
    | TypeRepr.Resolved left_type_constructor_id, TypeRepr.Resolved right_type_constructor_id -> TypeConstructorId.equal
      left_type_constructor_id
      right_type_constructor_id
    | _ -> IdentPath.equal left_name right_name
  in
  let mismatch left right = Unify_error (Typ_diagnostic.ExpectedActual {
    expected = TypePrinter.type_to_string left;
    actual = TypePrinter.type_to_string right
  }) in
  let pair_generation = Region.next_mark state.regions in
  let next_node_order =
    let order = ref 0 in
    fun () ->
      let current = !order in
      let () =
        order := current + 1
      in
      current
  in
  let node_order ty =
    let ty = TypeRepr.prune ty in
    if Int.equal (TypeRepr.aux_mark ty) pair_generation then
      TypeRepr.aux_order ty
    else
      let order = next_node_order () in
      let () =
        TypeRepr.set_aux_mark ty pair_generation;
        TypeRepr.set_aux_order ty order
      in
      order
  in
  let seen_pairs = Collections.HashSet.with_capacity 128 in
  let mark_pair_seen left right =
    let left_order = node_order left in
    let right_order = node_order right in
    let key =
      if left_order <= right_order then
        (left_order, right_order)
      else
        (right_order, left_order)
    in
    if Collections.HashSet.contains seen_pairs key then
      true
    else
      let () = Collections.HashSet.insert seen_pairs key |> ignore in
      false
  in
  let rec loop = function
    | [] -> ()
    | (left, right) :: rest ->
        let left = TypeRepr.prune left in
        let right = TypeRepr.prune right in
        if Std.Ptr.equal left right then
          loop rest
        else if mark_pair_seen left right then
          loop rest
        else
          match (TypeRepr.view left, TypeRepr.view right) with
          | (TypeRepr.Int, TypeRepr.Int)
          | (TypeRepr.Float, TypeRepr.Float)
          | (TypeRepr.Bool, TypeRepr.Bool)
          | (TypeRepr.String, TypeRepr.String)
          | (TypeRepr.Char, TypeRepr.Char)
          | (TypeRepr.Unit, TypeRepr.Unit) ->
              loop rest
          | (TypeRepr.Option left_element, TypeRepr.Option right_element)
          | (TypeRepr.Array left_element, TypeRepr.Array right_element)
          | (TypeRepr.List left_element, TypeRepr.List right_element)
          | (TypeRepr.Seq left_element, TypeRepr.Seq right_element) ->
              loop ((left_element, right_element) :: rest)
          | TypeRepr.Result (left_ok, left_error), TypeRepr.Result (right_ok, right_error) ->
              loop ((left_ok, right_ok) :: (left_error, right_error) :: rest)
          | (TypeRepr.Hole _, _)
          | (_, TypeRepr.Hole _) ->
              loop rest
          | TypeRepr.Named {
            type_constructor=left_type_constructor;
            name=left_name;
            arguments=left_arguments
          }, TypeRepr.Named {
            type_constructor=right_type_constructor;
            name=right_name;
            arguments=right_arguments
          } ->
              if
                not
                  (named_types_match left_type_constructor left_name right_type_constructor right_name)
              then
                raise (mismatch left right)
              else if List.length left_arguments != List.length right_arguments then
                raise (mismatch left right)
              else
                loop (List.rev_append (List.combine left_arguments right_arguments) rest)
          | TypeRepr.Tuple left_members, TypeRepr.Tuple right_members ->
              if List.length left_members != List.length right_members then
                raise
                  (Unify_error (Typ_diagnostic.TupleArityMismatch {
                    left = TypePrinter.type_to_string left;
                    right = TypePrinter.type_to_string right;
                    left_arity = List.length left_members;
                    right_arity = List.length right_members
                  }))
              else
                loop (List.rev_append (List.combine left_members right_members) rest)
          | TypeRepr.Arrow { label=left_label; lhs=left_arg; rhs=left_res }, TypeRepr.Arrow {
            label=right_label;
            lhs=right_arg;
            rhs=right_res
          } ->
              if not (labels_match left_label right_label) then
                raise (mismatch left right)
              else
                loop ((left_arg, right_arg) :: (left_res, right_res) :: rest)
          | TypeRepr.Var left_var, TypeRepr.Var right_var when left_var.id = right_var.id ->
              loop rest
          | (TypeRepr.Var var, _)
          | (_, TypeRepr.Var var) ->
              let (var_ty, other_ty) =
                match TypeRepr.view left with
                | TypeRepr.Var _ -> (left, right)
                | _ -> (right, left)
              in
              let level = TypeRepr.level var_ty in
              let occurs_generation = Region.next_mark state.regions in
              if
                TypeRepr.occurs_check
                  ~generation:occurs_generation
                  ~needle:var.id
                  ~minimum_level:level
                  other_ty
              then
                raise
                  (Unify_error (Typ_diagnostic.OccursCheckFailed {
                    variable_id = var.id;
                    in_type = TypePrinter.type_to_string other_ty
                  }))
              else
                (
                  let lower_generation = Region.next_mark state.regions in
                  let () =
                    TypeRepr.lower_level
                      ~generation:lower_generation
                      ~level
                      ~on_lower:(fun ty ->
                        Region.add_to_pool state.regions ~level:(TypeRepr.level ty) ty |> ignore)
                      other_ty
                  in
                  var.link <- Some other_ty;
                  loop rest
                )
          | _ ->
              raise (mismatch left right)
  in
  loop [ (left, right) ]

let try_unify = fun (state: state) ~origin left right ->
  try
    unify state ~origin left right;
    ()
  with
  | Unify_error mismatch -> add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch { mismatch_span = diagnostic_span origin; mismatch })

let is_recursive_binding_supported = fun (state: state) (binding: BodyArena.binding) ->
  match binding.name with
  | None -> false
  | Some _ -> (
      match SemanticTree.find_expr state.file binding.value_id with
      | Some value -> (
          match value.desc with
          | BodyArena.EFun _ -> true
          | _ -> false
        )
      | None -> false
    )

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
        (make_type
          state
          (TypeRepr.Arrow { label = TypeRepr.Nolabel; lhs = argument_ty; rhs = result_ty })) in
      let (rest_argument_types, final_result_ty) = constructor_pattern_argument_types
        state
        result_ty
        rest
        origin in
      (argument_ty :: rest_argument_types, final_result_ty)

let rec bind_pattern = fun (state: state) env pat_id expected_ty ->
  let normalize_bindings bindings = bindings |> Env.of_bindings |> Env.unique |> Env.render in
  let binding_names bindings = bindings |> List.map fst in
  let scheme_type = TypeScheme.body in
  let unify_or_pattern_bindings origin bindings alternatives =
    let expected_bindings = normalize_bindings bindings in
    let expected_names = binding_names expected_bindings in
    let rec loop current_bindings remaining =
      match remaining with
      | [] -> Some current_bindings
      | alternative_bindings :: rest ->
          let alternative_bindings = normalize_bindings alternative_bindings in
          let actual_names = binding_names alternative_bindings in
          if not (List.equal String.equal expected_names actual_names) then
            let () = add_or_pattern_bindings_mismatch
              state
              ~span:(diagnostic_span origin)
              ~expected_names
              ~actual_names in
            None
          else
            let () =
              List.iter2
                (fun (_, expected_scheme) (_, actual_scheme) ->
                  try_unify state ~origin (scheme_type expected_scheme) (scheme_type actual_scheme))
                current_bindings
                alternative_bindings
            in
            loop current_bindings rest
    in
    match loop expected_bindings alternatives with
    | Some _ -> Some bindings
    | None -> None
  in
  match SemanticTree.find_pattern state.file pat_id with
  | None -> []
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name ->
          [ generalized_pattern_binding state pat_id ~name expected_ty ]
      | BodyArena.PWildcard ->
          []
      | BodyArena.PInt _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.int in
          []
      | BodyArena.PFloat _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.float in
          []
      | BodyArena.PBool _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.bool in
          []
      | BodyArena.PString _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.string in
          []
      | BodyArena.PChar _ ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.char in
          []
      | BodyArena.PUnit ->
          let () = try_unify state ~origin:(origin_of_pattern state pat_id) expected_ty TypeRepr.unit_ in
          []
      | BodyArena.PTuple elements ->
          let element_types =
            List.map (fun _ -> fresh_var state) elements
          in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (make_type state (TypeRepr.Tuple element_types)) in
          List.map2 (bind_pattern state env) elements element_types |> List.flatten
      | BodyArena.POr alternatives -> (
          match alternatives with
          | [] -> []
          | alternative :: rest ->
              let origin = origin_of_pattern state pat_id in
              let bindings = bind_pattern state env alternative expected_ty in
              let alternative_bindings = rest
              |> List.map (fun alternative_id -> bind_pattern state env alternative_id expected_ty) in
              match unify_or_pattern_bindings origin bindings alternative_bindings with
              | Some bindings -> bindings
              | None -> []
        )
      | BodyArena.PConstructor { constructor; arguments } -> (
          match resolve_constructor_entry state env constructor ~expected_ty with
          | Some constructor_entry ->
              let origin = origin_of_pattern state pat_id in
              let constructor_ty = instantiate state (Env.Constructor_env.scheme constructor_entry) in
              let (argument_types, result_ty) = constructor_pattern_argument_types
                state
                constructor_ty
                arguments
                origin in
              let () = try_unify state ~origin expected_ty result_ty in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
          | None ->
              let argument_types =
                List.map (fun _ -> fresh_var state) arguments
              in
              List.map2 (bind_pattern state env) arguments argument_types |> List.flatten
        )
      | BodyArena.PRecord { fields; open_ } -> (
          let origin = origin_of_pattern state pat_id in
          let field_names =
            List.map (fun (field: BodyArena.record_pattern_field) -> field.label) fields
          in
          match resolve_record_decl
            env
            state
            ~field_names
            ~owner_hint:(owner_of_type state expected_ty)
            ~span:(diagnostic_span origin)
            ~context:Typ_diagnostic.RecordPattern with
          | Some record_decl ->
              let (owner_ty, field_types) = instantiate_record_decl state record_decl in
              let () = try_unify state ~origin expected_ty owner_ty in
              let missing_fields =
                if open_ then
                  []
                else
                  Env.Label_env.field_names record_decl |> List.filter
                    (fun label_name ->
                      not
                        (
                          List.exists
                            (fun requested_name ->
                              String.equal (Env.Label_env.lookup_name requested_name) label_name)
                            field_names
                        ))
              in
              let () =
                if not (List.is_empty missing_fields) then
                  add_record_resolution_error
                    state
                    ~span:(diagnostic_span origin)
                    ~context:Typ_diagnostic.RecordPattern (Typ_diagnostic.MissingRecordFields missing_fields)
              in
              fields |> List.map
                (fun (field: BodyArena.record_pattern_field) ->
                  let field_ty =
                    match record_field_type field_types field.label with
                    | Some field_ty -> field_ty
                    | None -> fresh_hole state
                  in
                  bind_pattern state env field.pattern_id field_ty) |> List.flatten
          | None -> fields
          |> List.map
            (fun (field: BodyArena.record_pattern_field) ->
              bind_pattern state env field.pattern_id (fresh_hole state))
          |> List.flatten
        )
      | BodyArena.PList elements ->
          let element_ty = fresh_var state in
          let () = try_unify
            state
            ~origin:(origin_of_pattern state pat_id)
            expected_ty
            (make_type state (TypeRepr.List element_ty)) in
          elements
          |> List.map (fun element_id -> bind_pattern state env element_id element_ty)
          |> List.flatten
      | BodyArena.PAlias { pattern_id; alias } ->
          let bindings = bind_pattern state env pattern_id expected_ty in
          generalized_pattern_binding state pat_id ~name:alias expected_ty :: bindings
      | BodyArena.PPolyVariant { payload; _ } -> (
          match payload with
          | Some payload_id -> bind_pattern state env payload_id (fresh_hole state)
          | None -> []
        )
      | BodyArena.PUnsupported _ ->
          []
    )

let record_expr_trace = fun (state: state) expr_id origin_id env_before inferred_type ->
  if state.config.capture_traces then
    state.expr_traces <- (
      { Check_result.expr_id; origin_id; env_before = Env.render env_before; inferred_type }:
        Check_result.expr_trace
    )
    :: state.expr_traces

let add_application_label_mismatch = fun (state: state) ~expr_id ~expected_label arguments ->
  add_diagnostic
    state
    (Typ_diagnostic.ApplicationLabelMismatch {
      application_span = diagnostic_span (origin_of_expr state expr_id);
      expected_label = diagnostic_label_of_type_label expected_label;
      actual_labels = List.map
        (fun (argument: BodyArena.apply_argument) -> diagnostic_label_of_body_label argument.label)
        arguments
    })

let add_application_non_function = fun (state: state) ~expr_id current_ty ->
  add_diagnostic
    state
    (Typ_diagnostic.TypeMismatch {
      mismatch_span = diagnostic_span (origin_of_expr state expr_id);
      mismatch = Typ_diagnostic.ExpectedActual {
        expected = "function";
        actual = TypePrinter.type_to_string current_ty
      }
    })

let rec infer_match_case = fun (state: state) env scrutinee_ty result_ty (case: BodyArena.match_case) ->
  let bindings = bind_pattern state env case.pattern_id scrutinee_ty in
  let case_env = Env.extend env bindings in
  let () =
    match case.guard_id with
    | Some guard_id ->
        let guard_ty = infer_expr state case_env guard_id in
        try_unify state ~origin:(origin_of_expr state guard_id) guard_ty TypeRepr.bool
    | None -> ()
  in
  let case_ty = infer_expr state case_env case.body_id in
  try_unify state ~origin:(origin_of_expr state case.body_id) result_ty case_ty

and infer_record_expr = fun (state: state) env expr_id base_id fields ->
  let operation_span = diagnostic_span (origin_of_expr state expr_id) in
  let base_ty =
    match base_id with
    | Some base_id -> Some (infer_expr state env base_id)
    | None -> None
  in
  let field_names =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  let context =
    match base_id with
    | Some _ -> Typ_diagnostic.RecordUpdate
    | None -> Typ_diagnostic.RecordConstruction
  in
  match
    resolve_record_decl env state ~field_names
      ~owner_hint:((
        match base_ty with
        | Some base_ty -> owner_of_type state base_ty
        | None -> None
      ))
      ~span:operation_span
      ~context
  with
  | Some record_decl ->
      let (owner_ty, field_types) = instantiate_record_decl state record_decl in
      let () =
        match base_id, base_ty with
        | Some base_id, Some base_ty -> try_unify
          state
          ~origin:(origin_of_expr state base_id)
          base_ty
          owner_ty
        | _ -> ()
      in
      let missing_fields =
        match base_id with
        | Some _ -> []
        | None ->
            Env.Label_env.field_names record_decl |> List.filter
              (fun label_name ->
                not
                  (
                    List.exists
                      (fun requested_name ->
                        String.equal (Env.Label_env.lookup_name requested_name) label_name)
                      field_names
                  ))
      in
      let () =
        if not (List.is_empty missing_fields) then
          add_record_resolution_error
            state
            ~span:operation_span
            ~context
            (Typ_diagnostic.MissingRecordFields missing_fields)
      in
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let field_ty =
              match record_field_type field_types field.label with
              | Some field_ty -> field_ty
              | None -> fresh_hole state
            in
            let inferred_field_ty = infer_expr state env field.value_id in
            try_unify state ~origin:(origin_of_expr state field.value_id) field_ty inferred_field_ty)
          fields
      in
      owner_ty
  | None ->
      let () =
        List.iter
          (fun (field: BodyArena.record_expr_field) ->
            let _ = infer_expr state env field.value_id in
            ())
          fields
      in
      fresh_hole state

and infer_expr = fun (state: state) env expr_id ->
  match SemanticTree.find_expr state.file expr_id with
  | None -> fresh_hole state
  | Some expr ->
      let inferred_type =
        match expr.desc with
        | BodyArena.EVar name -> (
            match Env.lookup env name with
            | Some binding -> instantiate state (Binding.scheme binding)
            | None -> (
                match origin_of_expr state expr_id with
                | Some origin when String.equal origin.label "constructor_expression"
                || String.equal origin.label "constructor_path_expression" -> (
                    match resolve_constructor_without_expected env name with
                    | Some constructor_entry -> instantiate
                      state
                      (Env.Constructor_env.scheme constructor_entry)
                    | None ->
                        let hole = fresh_hole state in
                        let () = add_diagnostic
                          state
                          (Typ_diagnostic.UnboundName {
                            reference_span = diagnostic_span (origin_of_expr state expr_id);
                            name = IdentPath.to_string name
                          }) in
                        hole
                  )
                | _ ->
                    let hole = fresh_hole state in
                    let () = add_diagnostic
                      state
                      (Typ_diagnostic.UnboundName {
                        reference_span = diagnostic_span (origin_of_expr state expr_id);
                        name = IdentPath.to_string name
                      }) in
                    hole
              )
          )
        | BodyArena.EInt _ ->
            TypeRepr.int
        | BodyArena.EFloat _ ->
            TypeRepr.float
        | BodyArena.EBool _ ->
            TypeRepr.bool
        | BodyArena.EString _ ->
            TypeRepr.string
        | BodyArena.EChar _ ->
            TypeRepr.char
        | BodyArena.EUnit ->
            TypeRepr.unit_
        | BodyArena.ETuple elements ->
            make_type state (TypeRepr.Tuple (List.map (infer_expr state env) elements))
        | BodyArena.EArray elements ->
            let element_ty = fresh_var state in
            let () =
              List.iter
                (fun element_id ->
                  let inferred_element = infer_expr state env element_id in
                  try_unify state ~origin:(origin_of_expr state element_id) element_ty inferred_element)
                elements
            in
            make_type state (TypeRepr.Array element_ty)
        | BodyArena.ESequence elements -> (
            match List.rev elements with
            | [] -> TypeRepr.unit_
            | last_id :: reversed_prefix ->
                let prefix = List.rev reversed_prefix in
                let () =
                  List.iter
                    (fun element_id ->
                      let element_ty = infer_expr state env element_id in
                      try_unify state ~origin:(origin_of_expr state element_id) element_ty TypeRepr.unit_)
                    prefix
                in
                infer_expr state env last_id
          )
        | BodyArena.EFun (parameters, body_id) ->
            let rec lower_parameters env = function
              | [] -> infer_expr state env body_id
              | (parameter: BodyArena.function_parameter) :: rest ->
                  let arg_ty = fresh_var state in
                  let bindings = bind_pattern state env parameter.pattern_id arg_ty in
                  let body_ty = lower_parameters (Env.extend env bindings) rest in
                  make_type
                    state
                    (TypeRepr.Arrow {
                      label = type_label_of_body_label parameter.label;
                      lhs = arg_ty;
                      rhs = body_ty
                    })
            in
            lower_parameters env parameters
        | BodyArena.EApply (callee_id, arguments) ->
            let callee_ty = infer_expr state env callee_id in
            let rec apply_with_known_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | _ -> (
                  match view current_ty with
                  | TypeRepr.Arrow { label; lhs; rhs } -> (
                      match take_matching_argument label arguments with
                      | Some ((argument: BodyArena.apply_argument), rest_arguments) ->
                          let _ = infer_expr_against state env argument.value_id lhs in
                          apply_with_known_type rhs rest_arguments
                      | None -> (
                          match label with
                          | TypeRepr.Optional _ -> apply_with_known_type rhs arguments
                          | _ ->
                              let () = add_application_label_mismatch
                                state
                                ~expr_id
                                ~expected_label:label
                                arguments in
                              fresh_hole state
                        )
                    )
                  | TypeRepr.Var _
                  | TypeRepr.Hole _ ->
                      apply_from_unknown_type current_ty arguments
                  | _ ->
                      let () = add_application_non_function state ~expr_id current_ty in
                      fresh_hole state
                )
            and apply_from_unknown_type current_ty arguments =
              match arguments with
              | [] -> current_ty
              | (argument: BodyArena.apply_argument) :: rest ->
                  let argument_ty = infer_expr state env argument.value_id in
                  let result_ty = fresh_var state in
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state expr_id)
                    current_ty
                    (make_type
                      state
                      (TypeRepr.Arrow {
                        label = type_label_of_body_label argument.label;
                        lhs = argument_ty;
                        rhs = result_ty
                      })) in
                  apply_with_known_type result_ty rest
            in
            apply_with_known_type callee_ty arguments
        | BodyArena.ERecord { base_id; fields } ->
            infer_record_expr state env expr_id base_id fields
        | BodyArena.EFieldAccess { receiver_id; label } ->
            let receiver_ty = infer_expr state env receiver_id in
            let field_names = [ label ] in
            begin
              match resolve_record_decl
                env
                state
                ~field_names
                ~owner_hint:(owner_of_type state receiver_ty)
                ~span:(diagnostic_span (origin_of_expr state expr_id))
                ~context:Typ_diagnostic.RecordFieldAccess with
              | Some record_decl ->
                  let (owner_ty, field_types) = instantiate_record_decl state record_decl in
                  let () = try_unify state ~origin:(origin_of_expr state receiver_id) receiver_ty owner_ty in
                  begin
                    match record_field_type field_types label with
                    | Some field_ty -> field_ty
                    | None -> fresh_hole state
                  end
              | None -> fresh_hole state
            end
        | BodyArena.EIndex (collection_id, index_id) ->
            let collection_ty = infer_expr state env collection_id in
            let index_ty = infer_expr state env index_id in
            let element_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state index_id) index_ty TypeRepr.int in
            begin
              match view collection_ty with
              | TypeRepr.String -> TypeRepr.char
              | _ ->
                  let () = try_unify
                    state
                    ~origin:(origin_of_expr state collection_id)
                    collection_ty
                    (make_type state (TypeRepr.Array element_ty)) in
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
              TypeRepr.bool in
            let then_ty = infer_expr state env then_id in
            let else_ty = infer_expr state env else_id in
            let () = try_unify state ~origin:(origin_of_expr state expr_id) then_ty else_ty in
            then_ty
        | BodyArena.EMatch (scrutinee_id, cases) ->
            let scrutinee_ty = infer_expr state env scrutinee_id in
            let result_ty = fresh_var state in
            let () = List.iter (infer_match_case state env scrutinee_ty result_ty) cases in
            result_ty
        | BodyArena.ETry (body_id, cases) ->
            let body_ty = infer_expr state env body_id in
            let exn_ty = make_type
              state
              (TypeRepr.Named {
                type_constructor = TypeRepr.Resolved BuiltinTypeConstructors.exn_type_constructor_id;
                name = IdentPath.of_name "exn";
                arguments = []
              }) in
            let result_ty = fresh_var state in
            let () = try_unify state ~origin:(origin_of_expr state body_id) result_ty body_ty in
            let () = List.iter (infer_match_case state env exn_ty result_ty) cases in
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
            infer_expr state (Env.with_local_open env module_path) body_id
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

and infer_expr_against = fun (state: state) env expr_id expected_ty ->
  match SemanticTree.find_expr state.file expr_id with
  | Some expr -> (
      match expr.desc with
      | BodyArena.EVar name -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_expression"
          || String.equal origin.label "constructor_path_expression" -> (
              match resolve_constructor_entry state env name ~expected_ty with
              | Some constructor_entry ->
                  let inferred_type = instantiate
                    state
                    (Env.Constructor_env.scheme constructor_entry) in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
              | None ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | BodyArena.EApply (callee_id, arguments) -> (
          match origin_of_expr state expr_id with
          | Some origin when String.equal origin.label "constructor_apply_expression" -> (
              match SemanticTree.find_expr state.file callee_id with
              | Some { desc=BodyArena.EVar constructor; _ } -> (
                  match resolve_constructor_entry state env constructor ~expected_ty with
                  | Some constructor_entry ->
                      let callee_ty = instantiate
                        state
                        (Env.Constructor_env.scheme constructor_entry) in
                      let rec apply_with_known_type current_ty arguments =
                        match arguments with
                        | [] ->
                            let () = try_unify
                              state
                              ~origin:(origin_of_expr state expr_id)
                              expected_ty
                              current_ty in
                            current_ty
                        | (argument: BodyArena.apply_argument) :: rest -> (
                            match view current_ty with
                            | TypeRepr.Arrow { label; lhs; rhs } ->
                                if argument_matches_parameter_label label argument.label then
                                  let _ = infer_expr_against state env argument.value_id lhs in
                                  apply_with_known_type rhs rest
                                else
                                  let inferred_type = infer_expr state env expr_id in
                                  let () = try_unify
                                    state
                                    ~origin:(origin_of_expr state expr_id)
                                    expected_ty
                                    inferred_type in
                                  inferred_type
                            | _ ->
                                let inferred_type = infer_expr state env expr_id in
                                let () = try_unify
                                  state
                                  ~origin:(origin_of_expr state expr_id)
                                  expected_ty
                                  inferred_type in
                                inferred_type
                          )
                      in
                      apply_with_known_type callee_ty arguments
                  | None ->
                      let inferred_type = infer_expr state env expr_id in
                      let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                      inferred_type
                )
              | _ ->
                  let inferred_type = infer_expr state env expr_id in
                  let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
                  inferred_type
            )
          | _ ->
              let inferred_type = infer_expr state env expr_id in
              let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
              inferred_type
        )
      | _ ->
          let inferred_type = infer_expr state env expr_id in
          let () = try_unify state ~origin:(origin_of_expr state expr_id) expected_ty inferred_type in
          inferred_type
    )
  | None -> fresh_hole state

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
  with_local_level_gen state
    (fun frame ->
      let inferred_bindings =
        List.map
          (fun (binding: BodyArena.binding) ->
            let value_ty = infer_expr state env binding.value_id in
            let () = lower_expansive_binding_vars state frame binding.value_id value_ty in
            let bound_entries = bind_pattern state env binding.pattern_id value_ty in
            (binding, bound_entries))
          bindings
      in
      let generalized_bindings =
        let generalized_groups = generalize_entry_groups state frame
          (inferred_bindings |> List.map snd)
        in
        List.map2 (fun (binding, _) generalized -> (binding, generalized)) inferred_bindings generalized_groups
      in
      List.fold_left
        (fun env (_, entries) ->
          Env.extend env entries)
        env
        generalized_bindings)

and infer_recursive_group = fun (state: state) env bindings ->
  let unsupported_bindings =
    List.filter
      (fun (binding: BodyArena.binding) -> not (is_recursive_binding_supported state binding))
      bindings
  in
  if List.is_empty unsupported_bindings then
    with_local_level_gen state
      (fun frame ->
        let placeholders =
          bindings
          |> List.map (fun (binding: BodyArena.binding) -> (binding, fresh_var state))
          |> List.filter_map
            (fun ((binding: BodyArena.binding), ty) ->
              match binding.name with
              | Some name -> Some (generalized_pattern_binding state binding.pattern_id ~name ty)
              | None -> None)
        in
        let provisional_env = Env.extend env placeholders in
        let () =
          List.iter2
            (fun (binding: BodyArena.binding) placeholder_ty ->
              let value_ty = infer_expr state provisional_env binding.value_id in
              try_unify state ~origin:(origin_of_binding state binding) placeholder_ty value_ty)
            bindings
            (placeholders |> List.map (fun entry -> TypeScheme.body (Binding.scheme entry)))
        in
        let () =
          List.iter2
            (fun (binding: BodyArena.binding) entry ->
              lower_expansive_binding_vars
                state
                frame
                binding.value_id
                (TypeScheme.body (Binding.scheme entry)))
            bindings
            placeholders
        in
        let generalized =
          generalize_entry_groups state frame
            (placeholders |> List.map (fun entry -> [ entry ]))
          |> List.map List.hd
        in
        Env.extend env generalized)
  else
    let () =
      List.iter
        (fun (binding: BodyArena.binding) ->
          add_diagnostic
            state
            (Typ_diagnostic.RecursiveGroupRequiresSimpleVariableBinders {
              binding_span = diagnostic_span (origin_of_binding state binding)
            }))
        unsupported_bindings
    in
    List.fold_left
      (fun env (binding: BodyArena.binding) ->
        let placeholder = fresh_hole state in
        let bound_entries = bind_pattern state env binding.pattern_id placeholder in
        Env.extend env bound_entries)
      env
      bindings

let infer_file = fun ~config file ->
  let state = make_state ~config file in
  let initial_env = Env.bind
    (Env.bind (prelude_env state config) (ambient_env state config))
    (ambient_type_env state config) in
  let rec loop export_state type_decls scope = function
    | [] -> (export_state, type_decls)
    | item :: rest -> (
        match item with
        | ItemTree.Type type_item ->
            let introduced = type_item_env state type_item in
            let introduced_type_decls = [
              { FileSummary.scope_path = type_item.scope_path; declaration = type_item.declaration }
            ] in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty type_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:type_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:type_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = type_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let () = set_visible_type_decls state type_decls in
            loop export_state type_decls scope rest
        | ItemTree.Exception exception_item ->
            let introduced = exception_bindings state exception_item in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty exception_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:exception_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:exception_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = exception_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Value value_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:value_item.scope_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let env_after_item = infer_binding_group state item_env value_item.binding_ids in
            let introduced = Env.introduced_entries item_env env_after_item in
            let (export_state, scope) =
              if IdentPath.is_empty value_item.scope_path then
                (env_after_item, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:value_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:value_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = value_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.DeclaredValue declared_value_item ->
            let introduced = declared_value_bindings state declared_value_item in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let (export_state, scope) =
              if IdentPath.is_empty declared_value_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:declared_value_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:declared_value_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  {
                    Check_result.item_id = declared_value_item.item_id;
                    binding_names;
                    exports_after
                  }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Open open_item ->
            let scope = Env.register_open
              scope
              ~scope_path:open_item.scope_path
              ~module_path:open_item.module_path in
            let () =
              if state.config.capture_traces then
                let exports_after = Env.export config export_state |> Env.render in
                state.item_traces <- (
                  { Check_result.item_id = open_item.item_id; binding_names = []; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            loop export_state type_decls scope rest
        | ItemTree.Include include_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:include_item.scope_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export config export_state)
              else
                None
            in
            let introduced = Env.entries_for_include item_env include_item.module_path in
            let visible_type_decls = bind_type_decls config.ambient_type_decls type_decls in
            let introduced_type_decls = type_decls_for_include visible_type_decls include_item.module_path
            |> prefix_type_decls include_item.scope_path in
            let (export_state, scope) =
              if IdentPath.is_empty include_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:include_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:include_item.scope_path introduced
                )
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export config export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = include_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let () = set_visible_type_decls state type_decls in
            loop export_state type_decls scope rest
        | ItemTree.ModuleAlias module_alias_item ->
            let item_env = Env.for_item_scope export_state scope ~scope_path:module_alias_item.scope_path in
            let visible_exports_before =
              if state.config.capture_traces then
                Some (Env.export_with_forced_names
                  ~config:state.config
                  ~forced_export_names:state.forced_export_names
                  export_state)
              else
                None
            in
            let alias_export_names = Env.export_names_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path in
            let introduced = Env.entries_for_module_alias
              item_env
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path in
            let visible_type_decls = bind_type_decls config.ambient_type_decls type_decls in
            let introduced_type_decls = type_decls_for_module_alias
              visible_type_decls
              ~alias_name:module_alias_item.alias_name
              ~module_path:module_alias_item.module_path
            |> prefix_type_decls module_alias_item.scope_path in
            let (export_state, scope) =
              if IdentPath.is_empty module_alias_item.scope_path then
                (Env.bind export_state introduced, scope)
              else
                (
                  Env.bind_in_scope export_state ~scope_path:module_alias_item.scope_path introduced,
                  Env.register_entries scope ~scope_path:module_alias_item.scope_path introduced
                )
            in
            let forced_export_names =
              if IdentPath.is_empty module_alias_item.scope_path then
                alias_export_names
              else
                List.map
                  (fun name -> qualify_name module_alias_item.scope_path name |> IdentPath.to_string)
                  alias_export_names
            in
            let () =
              state.forced_export_names <- forced_export_names @ state.forced_export_names
            in
            let () =
              if state.config.capture_traces then
                let exports_after_env = Env.export_with_forced_names
                  ~config:state.config
                  ~forced_export_names:state.forced_export_names
                  export_state in
                let binding_names =
                  match visible_exports_before with
                  | Some visible_exports_before -> Env.introduced_names visible_exports_before exports_after_env
                  | None -> []
                in
                let exports_after = Env.render exports_after_env in
                state.item_traces <- (
                  { Check_result.item_id = module_alias_item.item_id; binding_names; exports_after }:
                    Check_result.item_trace
                )
                :: state.item_traces
            in
            let type_decls = bind_type_decls type_decls introduced_type_decls in
            let () = set_visible_type_decls state type_decls in
            loop export_state type_decls scope rest
        | ItemTree.Unsupported unsupported_item ->
            let () =
              if state.config.capture_traces then
                let exports_after = Env.export config export_state |> Env.render in
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
            loop export_state type_decls scope rest
      )
  in
  let (exports, type_decls) = loop initial_env [] Env.empty_scope (ItemTree.items file.item_tree) in
  {
    exports = Env.export_with_forced_names
      ~config:state.config
      ~forced_export_names:state.forced_export_names
      exports
    |> Env.render;
    type_decls;
    item_traces = List.rev state.item_traces;
    expr_traces = List.rev state.expr_traces;
    diagnostics = List.rev state.diagnostics;
  }
