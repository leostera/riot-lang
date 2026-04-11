open Std
open Std.Data
open Typ.Model
module Core = Core_ir

type error =
  | UnsupportedSourceKind of { kind: Source_unit.kind }
  | UnsupportedItem of { item_id: ItemArenaId.t; kind: string; scope_path: Core.Surface_path.t }
  | MissingBinding of { binding_id: BindingArenaId.t }
  | MissingExpr of { expr_id: ExprArenaId.t }
  | MissingPattern of { pattern_id: PatternArenaId.t }
  | UnsupportedBinding of { binding_id: BindingArenaId.t; reason: string }
  | UnsupportedPattern of { pattern_id: PatternArenaId.t; reason: string }
  | UnsupportedExpr of { expr_id: ExprArenaId.t; reason: string }
  | InvalidIntLiteral of { expr_id: ExprArenaId.t; literal: string }
  | InvalidFloatLiteral of { expr_id: ExprArenaId.t; literal: string }

type 'value validation = ('value, error list) result

type lowered_binding = {
  export: Core.Export.t option;
  item: Core.Init_item.t;
}

type top_level_pattern =
  | Named of string
  | Unit

type record_layout = {
  type_name: string;
  labels: string list;
}

type variant_constructor_layout = {
  constructor_id: ConstructorId.t;
  constructor_name: string;
  constructor_path: SurfacePath.t;
  tag_index: int;
  payload_arity: int;
}

type variant_layout = {
  type_name: string;
  type_constructor_id: TypeConstructorId.t;
  constructors: variant_constructor_layout list;
}

type variant_constructor_resolution =
  | ResolvedVariantConstructor of variant_layout * variant_constructor_layout
  | MissingVariantConstructor
  | AmbiguousVariantConstructor of (variant_layout * variant_constructor_layout) list

type lowered_variant_match_case = {
  layout: variant_layout;
  constructor: variant_constructor_layout;
  argument_pattern_ids: PatternArenaId.t list;
  body: Core.Expr.t;
}

let ok = fun value -> Ok value

let error = fun value -> Error [ value ]

let validation_map2 = fun left right f ->
  match (left, right) with
  | (Ok left, Ok right) -> Ok (f left right)
  | (Error left, Ok _) -> Error left
  | (Ok _, Error right) -> Error right
  | (Error left, Error right) -> Error (left @ right)

let validation_map3 = fun first second third f ->
  match (first, second, third) with
  | (Ok first, Ok second, Ok third) -> Ok (f first second third)
  | (Error first, Ok _, Ok _) -> Error first
  | (Ok _, Error second, Ok _) -> Error second
  | (Ok _, Ok _, Error third) -> Error third
  | (Error first, Error second, Ok _) -> Error (first @ second)
  | (Error first, Ok _, Error third) -> Error (first @ third)
  | (Ok _, Error second, Error third) -> Error (second @ third)
  | (Error first, Error second, Error third) -> Error (first @ second @ third)

let map_results = fun items f ->
  List.fold_right
    (fun item acc -> validation_map2 (f item) acc (fun item acc -> item :: acc))
    items
    (Ok [])

let source_kind_to_string = fun kind ->
  match kind with
  | Source_unit.Implementation -> "implementation"
  | Source_unit.Interface -> "interface"

let error_to_json = fun error ->
  match error with
  | UnsupportedSourceKind { kind } ->
      Json.obj
        [
          ("kind", Json.string "unsupported_source_kind");
          ("source_kind", Json.string (source_kind_to_string kind));
        ]
  | UnsupportedItem { item_id; kind; scope_path } ->
      let _ = item_id in
      Json.obj
        [
          ("kind", Json.string "unsupported_item");
          ("item_kind", Json.string kind);
          ("scope_path", Json.string (Core.Surface_path.to_string scope_path));
        ]
  | MissingBinding { binding_id } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "missing_binding") ]
  | MissingExpr { expr_id } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "missing_expr") ]
  | MissingPattern { pattern_id } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "missing_pattern") ]
  | UnsupportedBinding { binding_id; reason } ->
      let _ = binding_id in
      Json.obj [ ("kind", Json.string "unsupported_binding"); ("reason", Json.string reason); ]
  | UnsupportedPattern { pattern_id; reason } ->
      let _ = pattern_id in
      Json.obj [ ("kind", Json.string "unsupported_pattern"); ("reason", Json.string reason); ]
  | UnsupportedExpr { expr_id; reason } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "unsupported_expr"); ("reason", Json.string reason); ]
  | InvalidIntLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_int_literal"); ("literal", Json.string literal); ]
  | InvalidFloatLiteral { expr_id; literal } ->
      let _ = expr_id in
      Json.obj [ ("kind", Json.string "invalid_float_literal"); ("literal", Json.string literal); ]

let item_kind = fun (item: ItemTree.item) ->
  match item with
  | ItemTree.Type _ -> "type"
  | ItemTree.Exception _ -> "exception"
  | ItemTree.ExtensionConstructor _ -> "extension_constructor"
  | ItemTree.Value _ -> "value"
  | ItemTree.DeclaredValue _ -> "declared_value"
  | ItemTree.Open _ -> "open"
  | ItemTree.Include _ -> "include"
  | ItemTree.ModuleAlias _ -> "module_alias"
  | ItemTree.Unsupported _ -> "unsupported"

let label_to_string = fun label ->
  match label with
  | BodyArena.Positional -> "positional"
  | BodyArena.Labeled name -> format Format.[ str "labeled:"; str name ]
  | BodyArena.Optional name -> format Format.[ str "optional:"; str name ]

let fresh_destructure_name = fun prefix pattern_id ->
  format Format.[ str "__raml_"; str prefix; str "_"; int (PatternArenaId.to_int pattern_id) ]

let fresh_function_parameter_name = fun expr_id parameter_index ->
  format
    Format.[ str "__raml_param_"; int (ExprArenaId.to_int expr_id); str "_"; int parameter_index; ]

let fresh_record_base_name = fun expr_id ->
  format Format.[ str "__raml_record_"; int (ExprArenaId.to_int expr_id) ]

let fresh_lambda_name = fun expr_id ->
  format Format.[ str "__raml_lambda_"; int (ExprArenaId.to_int expr_id) ]

let fresh_match_scrutinee_name = fun expr_id ->
  format Format.[ str "__raml_match_"; int (ExprArenaId.to_int expr_id) ]

let core_surface_path_of_typ = fun path -> path

let unresolved_entity_id_of_typ_path = fun path ->
  path |> core_surface_path_of_typ |> Core.Entity_id.of_surface_path

let semantic_binding_id = fun ~name binding_id ->
  Core.Binding_id.local ~stamp:(BindingArenaId.to_int binding_id) ~name

let semantic_entity_id = fun ~name binding_id ->
  semantic_binding_id ~name binding_id |> Core.Entity_id.of_binding_id

let generated_expr_binding_id = fun ~name expr_id slot ->
  Core.Binding_id.local ~stamp:(-(1 + (ExprArenaId.to_int expr_id * 32) + slot)) ~name

let generated_expr_entity_id = fun ~name expr_id slot ->
  generated_expr_binding_id ~name expr_id slot |> Core.Entity_id.of_binding_id

let generated_pattern_binding_id = fun ~name pattern_id slot ->
  Core.Binding_id.local ~stamp:(-(1_000_000 + (PatternArenaId.to_int pattern_id * 32) + slot)) ~name

let generated_pattern_entity_id = fun ~name pattern_id slot ->
  generated_pattern_binding_id ~name pattern_id slot |> Core.Entity_id.of_binding_id

let wrap_nonrecursive_let = fun binding body ->
  Core.Expr.Let Core.Expr.{ rec_flag = Core.Rec_flag.Nonrecursive; bindings = [ binding ]; body }

let lower_direct_intrinsic_apply = fun path arguments ->
  match (SurfacePath.to_string path, arguments) with
  | ("ignore", [ argument ]) -> ok
    (Core.Expr.Sequence Core.Expr.{
      first = argument;
      second = Core.Expr.Constant Core.Constant.Unit
    })
  | _ ->
      let callee = ok (Core.Expr.Direct (unresolved_entity_id_of_typ_path path)) in
      validation_map2
        callee
        (ok arguments)
        (fun callee arguments -> Core.Expr.Apply Core.Expr.{ callee; arguments })

type direct_call_binding = {
  name: string;
  entity_id: Core.Entity_id.t;
  direct: bool;
}

let direct_call_binding = fun ~name ~entity_id ~direct -> { name; entity_id; direct }

let value_bound_binding = fun ~name ~entity_id -> direct_call_binding ~name ~entity_id ~direct:false

let expr_is_direct_callable = fun expr ->
  match expr with
  | Core.Expr.Lambda _ -> true
  | _ -> false

let bind_direct_call = fun env binding -> binding :: env

let bind_direct_calls = fun env bindings ->
  List.fold_left bind_direct_call env bindings

let lookup_direct_call = fun env name ->
  env |> List.find_map
    (fun (binding: direct_call_binding) ->
      if String.equal binding.name name then
        Some binding
      else
        None)

let unresolved_bare_name = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some _ -> None
  | None ->
      if Core.Entity_id.is_bare entity_id then
        Core.Entity_id.bare_name entity_id
      else
        None

let reclassify_entity_id = fun env entity_id ->
  match unresolved_bare_name entity_id with
  | Some bare_name -> (
      match lookup_direct_call env bare_name with
      | Some binding -> binding.entity_id
      | None -> entity_id
    )
  | None -> entity_id

let reclassify_direct_callee = fun env entity_id ->
  match unresolved_bare_name entity_id with
  | None -> Core.Expr.Direct entity_id
  | Some bare_name -> (
      match lookup_direct_call env bare_name with
      | Some binding when binding.direct -> Core.Expr.Direct binding.entity_id
      | Some binding -> Core.Expr.Indirect (Core.Expr.Var binding.entity_id)
      | None -> Core.Expr.Direct entity_id
    )

let direct_call_binding_of_core_binding = fun (binding: Core.Expr.binding) ->
  direct_call_binding
    ~name:binding.name
    ~entity_id:binding.entity_id
    ~direct:(expr_is_direct_callable binding.expr)

let direct_call_binding_of_init_item = fun item ->
  match item with
  | Core.Init_item.Binding binding -> Some (direct_call_binding
    ~name:binding.name
    ~entity_id:binding.entity_id
    ~direct:(expr_is_direct_callable binding.expr))
  | Core.Init_item.Eval _ -> None

let rec reclassify_expr = fun env expr ->
  match expr with
  | Core.Expr.Constant _ ->
      expr
  | Core.Expr.Var entity_id ->
      Core.Expr.Var (reclassify_entity_id env entity_id)
  | Core.Expr.Apply apply ->
      let callee =
        match apply.callee with
        | Core.Expr.Direct entity_id -> reclassify_direct_callee env entity_id
        | Core.Expr.Indirect callee -> Core.Expr.Indirect (reclassify_expr env callee)
      in
      let arguments = List.map (reclassify_expr env) apply.arguments in
      Core.Expr.Apply Core.Expr.{ callee; arguments }
  | Core.Expr.Lambda lambda ->
      let env =
        bind_direct_calls
          env
          (List.map
            (fun (param: Core.Expr.param) -> value_bound_binding ~name:param.name ~entity_id:param.entity_id)
            lambda.params)
      in
      Core.Expr.Lambda Core.Expr.{ lambda with body = reclassify_expr env lambda.body }
  | Core.Expr.Let let_ ->
      let bindings = List.map direct_call_binding_of_core_binding let_.bindings in
      let env_for_bindings =
        match let_.rec_flag with
        | Core.Rec_flag.Recursive -> bind_direct_calls env bindings
        | Core.Rec_flag.Nonrecursive -> env
      in
      let rewritten_bindings =
        List.map
          (fun (binding: Core.Expr.binding) ->
            { binding with expr = reclassify_expr env_for_bindings binding.expr })
          let_.bindings
      in
      let env_for_body = bind_direct_calls env bindings in
      let body = reclassify_expr env_for_body let_.body in
      Core.Expr.Let Core.Expr.{ let_ with bindings = rewritten_bindings; body }
  | Core.Expr.Sequence sequence ->
      Core.Expr.Sequence Core.Expr.{
        first = reclassify_expr env sequence.first;
        second = reclassify_expr env sequence.second
      }
  | Core.Expr.Tuple tuple ->
      Core.Expr.Tuple (List.map (reclassify_expr env) tuple)
  | Core.Expr.Tuple_get tuple_get ->
      Core.Expr.Tuple_get Core.Expr.{ tuple_get with tuple = reclassify_expr env tuple_get.tuple }
  | Core.Expr.Record record ->
      Core.Expr.Record (List.map
        (fun (field: Core.Expr.record_field) ->
          Core.Expr.{ field with value = reclassify_expr env field.value })
        record)
  | Core.Expr.Record_get record_get ->
      Core.Expr.Record_get Core.Expr.{
        record_get
        with record = reclassify_expr env record_get.record
      }
  | Core.Expr.If_then_else if_then_else ->
      Core.Expr.If_then_else Core.Expr.{
        condition = reclassify_expr env if_then_else.condition;
        then_ = reclassify_expr env if_then_else.then_;
        else_ = reclassify_expr env if_then_else.else_
      }
  | Core.Expr.Primitive primitive ->
      Core.Expr.Primitive Core.Expr.{
        primitive
        with arguments = List.map (reclassify_expr env) primitive.arguments
      }

let reclassify_binding_group = fun env (group: Core.Binding_group.t) ->
  let bindings = List.filter_map direct_call_binding_of_init_item group.items in
  let env_for_items =
    match group.rec_flag with
    | Core.Rec_flag.Recursive -> bind_direct_calls env bindings
    | Core.Rec_flag.Nonrecursive -> env
  in
  let items =
    List.map
      (fun item ->
        match item with
        | Core.Init_item.Binding binding -> Core.Init_item.Binding Core.Binding.{
          binding
          with expr = reclassify_expr env_for_items binding.expr
        }
        | Core.Init_item.Eval expr -> Core.Init_item.Eval (reclassify_expr env_for_items expr))
      group.items
  in
  ({ group with items }, bind_direct_calls env bindings)

let reclassify_compilation_unit = fun (compilation_unit: Core.Compilation_unit.t) ->
  let init, _ =
    List.fold_left
      (fun (groups, env) group ->
        let group, env = reclassify_binding_group env group in
        (groups @ [ group ], env))
      ([], [])
      compilation_unit.init
  in
  { compilation_unit with init }

let record_layouts = fun (semantic_tree: SemanticTree.file) ->
  List.fold_right
    (fun item acc ->
      match item with
      | ItemTree.Type type_item when not (List.is_empty type_item.declaration.labels) -> {
        type_name = type_item.declaration.type_name;
        labels = List.map (fun (label: TypeDecl.label) -> label.name) type_item.declaration.labels
      }
      :: acc
      | _ -> acc)
    (ItemTree.items semantic_tree.item_tree)
    []

let constructor_payload_arity =
  let rec arity_of_type type_ =
    match TypeRepr.view type_ with
    | TypeRepr.Arrow { rhs; _ } -> 1 + arity_of_type rhs
    | _ -> 0
  in
  fun (constructor: TypeDecl.constructor) -> arity_of_type (TypeScheme.body constructor.scheme)

let constructor_path = fun scope_path constructor_name ->
  if SurfacePath.is_empty scope_path then
    SurfacePath.of_name constructor_name
  else
    SurfacePath.append_name scope_path constructor_name

let variant_layout_of_type_decl = fun scope_path (declaration: TypeDecl.t) ->
  if List.is_empty declaration.constructors then
    None
  else
    Some {
      type_name = declaration.type_name;
      type_constructor_id = declaration.type_constructor_id;
      constructors =
        declaration.constructors |> List.mapi
          (fun tag_index (constructor: TypeDecl.constructor) ->
            {
              constructor_id = constructor.constructor_id;
              constructor_name = constructor.name;
              constructor_path = constructor_path scope_path constructor.name;
              tag_index;
              payload_arity = constructor_payload_arity constructor;
            });
    }

let constructor_has_payload = fun (constructor: variant_constructor_layout) ->
  constructor.payload_arity > 0

let prelude_variant_type_names = [ "list"; "option"; "result" ]

let prelude_variant_layouts = fun () ->
  Typ.Config.default.ambient_type_decls |> List.filter_map
    (fun (type_decl: FileSummary.type_decl) ->
      if List.exists (String.equal type_decl.declaration.type_name) prelude_variant_type_names then
        variant_layout_of_type_decl type_decl.scope_path type_decl.declaration
      else
        None)

let variant_layouts = fun (semantic_tree: SemanticTree.file) ->
  List.fold_right
    (fun item acc ->
      match item with
      | ItemTree.Type type_item when not (List.is_empty type_item.declaration.constructors) -> (
          match variant_layout_of_type_decl type_item.scope_path type_item.declaration with
          | Some layout -> layout :: acc
          | None -> acc
        )
      | _ -> acc)
    (ItemTree.items semantic_tree.item_tree)
    (prelude_variant_layouts ())

let variant_constructor_candidates = fun semantic_tree constructor_path ->
  List.fold_right
    (fun (layout: variant_layout) acc ->
      layout.constructors |> List.filter_map
        (fun (constructor: variant_constructor_layout) ->
          if SurfacePath.equal constructor.constructor_path constructor_path then
            Some (layout, constructor)
          else
            None) |> fun matches -> matches @ acc)
    (variant_layouts semantic_tree)
    []

let render_variant_constructor_names = fun constructors ->
  constructors
  |> List.map
    (fun ((layout: variant_layout), (constructor: variant_constructor_layout)) ->
      format
        Format.[
          str (SurfacePath.to_string constructor.constructor_path);
          str " from ";
          str layout.type_name;
        ])
  |> String.concat ", "

let resolve_variant_constructor = fun semantic_tree constructor_path ->
  match variant_constructor_candidates semantic_tree constructor_path with
  | [ (layout, constructor) ] -> ResolvedVariantConstructor (layout, constructor)
  | [] -> MissingVariantConstructor
  | candidates -> AmbiguousVariantConstructor candidates

let record_labels_match_exactly = fun expected actual ->
  List.length expected = List.length actual && List.for_all
    (fun label ->
      List.exists (String.equal label) actual)
    expected

let record_labels_are_subset = fun ~subset ~superset ->
  List.for_all
    (fun label ->
      List.exists (String.equal label) superset)
    subset

let render_record_labels = fun labels ->
  format Format.[ str "{"; str (String.concat ", " labels); str "}" ]

let resolve_record_layout = fun semantic_tree ~expr_id ~matches ~description ->
  match List.filter
    (fun (layout: record_layout) -> matches layout.labels)
    (record_layouts semantic_tree) with
  | [ layout ] -> ok layout
  | [] -> error
    (UnsupportedExpr {
      expr_id;
      reason = format
        Format.[
          str description;
          str " does not resolve to a visible immutable record declaration";
        ]
    })
  | layouts -> error
    (UnsupportedExpr {
      expr_id;
      reason = format
        Format.[
          str description;
          str " is ambiguous across visible immutable record declarations: ";
          str
            (String.concat ", " (List.map (fun (layout: record_layout) -> layout.type_name) layouts));
        ]
    })

let resolve_record_construction_layout = fun semantic_tree ~expr_id fields ->
  let labels =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  resolve_record_layout
    semantic_tree
    ~expr_id
    ~matches:(record_labels_match_exactly labels)
    ~description:(format Format.[ str "record literal "; str (render_record_labels labels) ])

let resolve_record_update_layout = fun semantic_tree ~expr_id fields ->
  let labels =
    List.map (fun (field: BodyArena.record_expr_field) -> field.label) fields
  in
  resolve_record_layout
    semantic_tree
    ~expr_id
    ~matches:(fun candidate_labels -> record_labels_are_subset ~subset:labels ~superset:candidate_labels)
    ~description:(format Format.[ str "record update "; str (render_record_labels labels) ])

let resolve_record_field_layout = fun semantic_tree ~expr_id label ->
  resolve_record_layout semantic_tree ~expr_id
    ~matches:(fun labels ->
      List.exists (String.equal label) labels)
    ~description:(format Format.[ str "record field `"; str label; str "`" ])

let resolve_variant_constructor_pattern = fun semantic_tree ~pattern_id constructor_path ->
  match resolve_variant_constructor semantic_tree constructor_path with
  | ResolvedVariantConstructor (layout, constructor) -> ok (layout, constructor)
  | MissingVariantConstructor -> error
    (UnsupportedPattern {
      pattern_id;
      reason = format
        Format.[
          str "constructor `";
          str (SurfacePath.to_string constructor_path);
          str "` does not resolve to a visible ordinary variant declaration";
        ]
    })
  | AmbiguousVariantConstructor candidates -> error
    (UnsupportedPattern {
      pattern_id;
      reason = format
        Format.[
          str "constructor `";
          str (SurfacePath.to_string constructor_path);
          str "` is ambiguous across visible ordinary variant declarations: ";
          str (render_variant_constructor_names candidates);
        ]
    })

let variant_tag_expr = fun scrutinee ->
  Core.Expr.Tuple_get Core.Expr.{ tuple = scrutinee; index = 0 }

let variant_payload_expr = fun scrutinee ->
  Core.Expr.Tuple_get Core.Expr.{ tuple = scrutinee; index = 1 }

let variant_constructor_payload = fun payloads ->
  match payloads with
  | [] -> None
  | [ payload ] -> Some payload
  | _ -> Some (Core.Expr.Tuple payloads)

let variant_constructor_expr = fun (constructor: variant_constructor_layout) payloads ->
  let fields =
    match variant_constructor_payload payloads with
    | None -> [ Core.Expr.Constant (Core.Constant.Int constructor.tag_index) ]
    | Some payload -> [ Core.Expr.Constant (Core.Constant.Int constructor.tag_index); payload; ]
  in
  Core.Expr.Tuple fields

let variant_payload_argument_expr = fun scrutinee index ->
  let payload = variant_payload_expr scrutinee in
  Core.Expr.Tuple_get Core.Expr.{ tuple = payload; index }

let record_label_index = fun (layout: record_layout) label ->
  let rec loop index = function
    | [] -> None
    | current :: rest ->
        if String.equal current label then
          Some index
        else
          loop (index + 1) rest
  in
  loop 0 layout.labels

let lowered_record_field = fun fields label ->
  List.find_opt
    (fun (field_label, _) ->
      String.equal field_label label)
    fields |> Option.map snd

let keep_runtime_item = fun item ->
  match item with
  | ItemTree.Type _ -> false
  | ItemTree.DeclaredValue _ -> false
  | ItemTree.Open _ -> false
  | _ -> true

let lower_var_pattern = fun semantic_tree pattern_id ->
  match SemanticTree.find_pattern semantic_tree pattern_id with
  | None -> error (MissingPattern { pattern_id })
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name -> ok name
      | _ -> error
        (UnsupportedPattern {
          pattern_id;
          reason = "only variable binders are supported in the first Typ -> Raml lowering slice"
        })
    )

let lower_top_level_pattern = fun semantic_tree pattern_id ->
  match SemanticTree.find_pattern semantic_tree pattern_id with
  | None -> error (MissingPattern { pattern_id })
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name -> ok (Named name)
      | BodyArena.PUnit -> ok Unit
      | _ -> error
        (UnsupportedPattern {
          pattern_id;
          reason = "only variable and unit top-level binders are supported in the first Typ -> Raml lowering slice"
        })
    )

let rec bind_pattern = fun semantic_tree pattern_id value body ->
  match SemanticTree.find_pattern semantic_tree pattern_id with
  | None -> error (MissingPattern { pattern_id })
  | Some pattern -> (
      match pattern.desc with
      | BodyArena.PVar name ->
          ok
            (wrap_nonrecursive_let
              Core.Expr.{
                entity_id = generated_pattern_entity_id ~name pattern_id 0;
                name;
                expr = value
              }
              body)
      | BodyArena.PTuple element_ids ->
          let tuple_name = fresh_destructure_name "tuple" pattern_id in
          let tuple_var = Core.Expr.Var (Core.Entity_id.of_name tuple_name) in
          let indexed_elements =
            List.mapi (fun index child_pattern_id -> (index, child_pattern_id)) element_ids
          in
          let body =
            List.fold_right
              (fun (index, child_pattern_id) body ->
                Result.and_then
                  body
                  (fun body ->
                    bind_pattern
                      semantic_tree
                      child_pattern_id
                      (Core.Expr.Tuple_get Core.Expr.{ tuple = tuple_var; index })
                      body))
              indexed_elements
              (ok body)
          in
          Result.map
            (fun body ->
              wrap_nonrecursive_let
                Core.Expr.{
                  entity_id = generated_pattern_entity_id ~name:tuple_name pattern_id 1;
                  name = tuple_name;
                  expr = value
                }
                body)
            body
      | _ ->
          error
            (UnsupportedPattern {
              pattern_id;
              reason = "only variable and tuple binders are supported in the current Typ -> Raml lowering slice"
            })
    )

let rec lower_function = fun semantic_tree ~name expr_id parameters body_id ->
  let lower_parameter parameter_index (parameter: BodyArena.function_parameter) =
    if Option.is_some parameter.default_value_id then
      error
        (UnsupportedExpr { expr_id; reason = "default-valued function parameters are not supported" })
    else
      match parameter.label with
      | BodyArena.Positional -> (
          match SemanticTree.find_pattern semantic_tree parameter.pattern_id with
          | None -> error (MissingPattern { pattern_id = parameter.pattern_id })
          | Some pattern -> (
              match pattern.desc with
              | BodyArena.PVar name ->
                  ok
                    (Core.Expr.{
                      entity_id = generated_expr_entity_id ~name expr_id parameter_index;
                      name
                    },
                    None)
              | BodyArena.PTuple _ ->
                  let name = fresh_function_parameter_name expr_id parameter_index in
                  ok
                    (Core.Expr.{
                      entity_id = generated_expr_entity_id ~name expr_id parameter_index;
                      name
                    },
                    Some parameter.pattern_id)
              | _ ->
                  error
                    (UnsupportedPattern {
                      pattern_id = parameter.pattern_id;
                      reason = "only variable and tuple function parameters are supported in the current Typ -> Raml lowering slice"
                    })
            )
        )
      | label -> error
        (UnsupportedExpr {
          expr_id;
          reason = format
            Format.[
              str "non-positional function parameters are not supported: ";
              str (label_to_string label);
            ]
        })
  in
  let params =
    map_results
      (parameters |> List.mapi (fun index parameter -> (index, parameter)))
      (fun (index, parameter) -> lower_parameter index parameter)
  in
  let body = lower_expr semantic_tree body_id in
  let _ = name in
  Result.and_then (validation_map2 params body (fun params body -> (params, body)))
    (fun (params, body) ->
      let body =
        List.fold_right
          (fun ((param_name: Core.Expr.param), pattern_id) body ->
            Result.and_then body
              (fun body ->
                match pattern_id with
                | None -> ok body
                | Some pattern_id -> bind_pattern
                  semantic_tree
                  pattern_id
                  (Core.Expr.Var param_name.entity_id)
                  body))
          params
          (ok body)
      in
      Result.map (fun body -> Core.Expr.{ params = List.map fst params; body }) body)

and lower_sequence_elements = fun semantic_tree ~expr_id element_ids ->
  match element_ids with
  | [] ->
      let _ = expr_id in
      ok (Core.Expr.Constant Core.Constant.Unit)
  | [ element_id ] ->
      lower_expr semantic_tree element_id
  | first_id :: rest ->
      validation_map2
        (lower_expr semantic_tree first_id)
        (lower_sequence_elements semantic_tree ~expr_id rest)
        (fun first second -> Core.Expr.Sequence Core.Expr.{ first; second })

and list_pattern_case = fun pattern_id elements ->
  match elements with
  | [] -> ok (SurfacePath.of_name "[]", [])
  | _ :: _ -> error
    (UnsupportedPattern {
      pattern_id;
      reason = "non-empty list literal patterns are outside the first prelude list lowering slice"
    })

and lower_closed_variant_match_case = fun semantic_tree ~match_expr_id (case: BodyArena.match_case) ->
  match case.guard_id with
  | Some _ -> error
    (UnsupportedExpr {
      expr_id = match_expr_id;
      reason = "match guards are outside the first closed ordinary-variant lowering slice"
    })
  | None -> (
      match SemanticTree.find_pattern semantic_tree case.pattern_id with
      | None -> error (MissingPattern { pattern_id = case.pattern_id })
      | Some pattern -> (
          match pattern.desc with
          | BodyArena.PConstructor { constructor; arguments } ->
              Result.and_then (resolve_variant_constructor_pattern
                semantic_tree
                ~pattern_id:case.pattern_id
                constructor)
                (fun (layout, constructor_layout) ->
                  let expected = constructor_layout.payload_arity in
                  let actual = List.length arguments in
                  if Int.equal expected actual then
                    Result.map
                      (fun body ->
                        {
                          layout;
                          constructor = constructor_layout;
                          argument_pattern_ids = arguments;
                          body
                        })
                      (lower_expr semantic_tree case.body_id)
                  else
                    error
                      (
                        UnsupportedPattern {
                          pattern_id = case.pattern_id;
                          reason =
                            format
                              Format.[
                                str "constructor pattern `";
                                str (SurfacePath.to_string constructor);
                                str "` must bind exactly ";
                                int expected;
                                str " payload ";
                                str
                                  (
                                    if Int.equal expected 1 then
                                      "pattern"
                                    else
                                      "patterns"
                                  );
                                str " in the current closed ordinary-variant lowering slice";
                              ];
                        }
                      ))
          | BodyArena.PList elements ->
              Result.and_then (list_pattern_case case.pattern_id elements)
                (fun (constructor, arguments) ->
                  Result.and_then (resolve_variant_constructor_pattern
                    semantic_tree
                    ~pattern_id:case.pattern_id
                    constructor)
                    (fun (layout, constructor_layout) ->
                      let expected = constructor_layout.payload_arity in
                      let actual = List.length arguments in
                      if Int.equal expected actual then
                        Result.map
                          (fun body ->
                            {
                              layout;
                              constructor = constructor_layout;
                              argument_pattern_ids = arguments;
                              body
                            })
                          (lower_expr semantic_tree case.body_id)
                      else
                        error
                          (
                            UnsupportedPattern {
                              pattern_id = case.pattern_id;
                              reason =
                                format
                                  Format.[
                                    str "constructor pattern `";
                                    str (SurfacePath.to_string constructor);
                                    str "` must bind exactly ";
                                    int expected;
                                    str " payload ";
                                    str
                                      (
                                        if Int.equal expected 1 then
                                          "pattern"
                                        else
                                          "patterns"
                                      );
                                    str " in the current closed ordinary-variant lowering slice";
                                  ];
                            }
                          )))
          | _ -> error
            (UnsupportedPattern {
              pattern_id = case.pattern_id;
              reason = "only constructor and [] list match cases are supported in the first closed ordinary-variant lowering slice"
            })
        )
    )

and lower_closed_variant_match = fun semantic_tree ~expr_id scrutinee_id cases ->
  if List.is_empty cases then
    error
      (UnsupportedExpr {
        expr_id;
        reason = "empty match expressions are outside the first closed ordinary-variant lowering slice"
      })
  else
    Result.and_then (validation_map2
      (lower_expr semantic_tree scrutinee_id)
      (map_results cases (lower_closed_variant_match_case semantic_tree ~match_expr_id:expr_id))
      (fun scrutinee lowered_cases -> (scrutinee, lowered_cases)))
      (fun (scrutinee, lowered_cases) ->
        match lowered_cases with
        | [] -> error
          (UnsupportedExpr {
            expr_id;
            reason = "empty match expressions are outside the first closed ordinary-variant lowering slice"
          })
        | first_case :: _ ->
            let all_same_layout =
              List.for_all
                (fun (case: lowered_variant_match_case) ->
                  TypeConstructorId.equal case.layout.type_constructor_id first_case.layout.type_constructor_id)
                lowered_cases
            in
            if not all_same_layout then
              error
                (UnsupportedExpr {
                  expr_id;
                  reason = "closed ordinary-variant matches must use constructors from one visible variant declaration"
                })
            else
              let rec find_duplicate seen = function
                | [] -> None
                | (case: lowered_variant_match_case) :: rest ->
                    if List.exists
                        (fun (seen_case: lowered_variant_match_case) ->
                          ConstructorId.equal seen_case.constructor.constructor_id case.constructor.constructor_id)
                        seen then
                      Some case.constructor.constructor_name
                    else
                      find_duplicate (case :: seen) rest
              in
              (
                match find_duplicate [] lowered_cases with
                | Some constructor_name -> error
                  (UnsupportedExpr {
                    expr_id;
                    reason = format
                      Format.[
                        str "closed ordinary-variant matches cannot repeat constructor `";
                        str constructor_name;
                        str "` in the current Typ -> Raml lowering slice";
                      ]
                  })
                | None ->
                    let missing_constructors =
                      first_case.layout.constructors
                      |> List.filter
                        (fun (constructor: variant_constructor_layout) ->
                          not
                            (
                              List.exists
                                (fun (case: lowered_variant_match_case) ->
                                  ConstructorId.equal case.constructor.constructor_id constructor.constructor_id)
                                lowered_cases
                            ))
                    in
                    if not (List.is_empty missing_constructors) then
                      error
                        (UnsupportedExpr {
                          expr_id;
                          reason = format
                            Format.[
                              str "closed ordinary-variant matches must cover every constructor of variant type ";
                              str first_case.layout.type_name;
                              str "; missing: ";
                              str
                                (String.concat
                                  ", "
                                  (List.map
                                    (fun (constructor: variant_constructor_layout) -> constructor.constructor_name)
                                    missing_constructors));
                            ]
                        })
                    else
                      let scrutinee_name = fresh_match_scrutinee_name expr_id in
                      let scrutinee_entity_id = generated_expr_entity_id
                        ~name:scrutinee_name
                        expr_id
                        16 in
                      let scrutinee_var = Core.Expr.Var scrutinee_entity_id in
                      let lower_case_body (case: lowered_variant_match_case) =
                        match case.argument_pattern_ids with
                        | [] -> ok case.body
                        | [ payload_pattern_id ] -> bind_pattern
                          semantic_tree
                          payload_pattern_id
                          (variant_payload_expr scrutinee_var)
                          case.body
                        | _ ->
                            List.fold_right
                              (fun (index, payload_pattern_id) body ->
                                Result.and_then
                                  body
                                  (fun body ->
                                    bind_pattern
                                      semantic_tree
                                      payload_pattern_id
                                      (variant_payload_argument_expr scrutinee_var index)
                                      body))
                              (List.mapi
                                (fun index payload_pattern_id -> (index, payload_pattern_id))
                                case.argument_pattern_ids)
                              (ok case.body)
                      in
                      let case_bodies =
                        map_results lowered_cases
                          (fun (case: lowered_variant_match_case) ->
                            Result.map
                              (fun body -> (case.constructor.tag_index, body))
                              (lower_case_body case))
                      in
                      Result.map
                        (fun case_bodies ->
                          let rec build = function
                            | [] -> Core.Expr.Constant Core.Constant.Unit
                            | [ (_, body) ] -> body
                            | (tag_index, body) :: rest -> Core.Expr.If_then_else Core.Expr.{
                              condition = Core.Expr.Primitive Core.Expr.{
                                primitive = Core.Primitive.Equal;
                                arguments = [
                                  variant_tag_expr scrutinee_var;
                                  Core.Expr.Constant (Core.Constant.Int tag_index);
                                ]
                              };
                              then_ = body;
                              else_ = build rest
                            }
                          in
                          wrap_nonrecursive_let
                            Core.Expr.{
                              entity_id = scrutinee_entity_id;
                              name = scrutinee_name;
                              expr = scrutinee
                            }
                            (build case_bodies))
                        case_bodies
              ))

and lower_expr = fun semantic_tree expr_id ->
  match SemanticTree.find_expr semantic_tree expr_id with
  | None -> error (MissingExpr { expr_id })
  | Some expr -> (
      match expr.desc with
      | BodyArena.EUnit ->
          ok (Core.Expr.Constant Core.Constant.Unit)
      | BodyArena.EBool value ->
          ok (Core.Expr.Constant (Core.Constant.Bool value))
      | BodyArena.EInt literal -> (
          match int_of_string_opt literal with
          | Some value -> ok (Core.Expr.Constant (Core.Constant.Int value))
          | None -> error (InvalidIntLiteral { expr_id; literal })
        )
      | BodyArena.EFloat literal -> (
          match float_of_string_opt literal with
          | Some value -> ok (Core.Expr.Constant (Core.Constant.Float value))
          | None -> error (InvalidFloatLiteral { expr_id; literal })
        )
      | BodyArena.EString value ->
          ok (Core.Expr.Constant (Core.Constant.String value))
      | BodyArena.EVar path -> (
          match resolve_variant_constructor semantic_tree path with
          | ResolvedVariantConstructor (_layout, constructor) ->
              if constructor_has_payload constructor then
                error
                  (
                    UnsupportedExpr {
                      expr_id;
                      reason =
                        format
                          Format.[
                            str "constructor `";
                            str (SurfacePath.to_string path);
                            str "` requires ";
                            int constructor.payload_arity;
                            str " positional ";
                            str
                              (
                                if Int.equal constructor.payload_arity 1 then
                                  "argument"
                                else
                                  "arguments"
                              );
                            str " in the current closed ordinary-variant lowering slice";
                          ];
                    }
                  )
              else
                ok (variant_constructor_expr constructor [])
          | MissingVariantConstructor -> ok (Core.Expr.Var (unresolved_entity_id_of_typ_path path))
          | AmbiguousVariantConstructor candidates -> error
            (UnsupportedExpr {
              expr_id;
              reason = format
                Format.[
                  str "constructor `";
                  str (SurfacePath.to_string path);
                  str "` is ambiguous across visible ordinary variant declarations: ";
                  str (render_variant_constructor_names candidates);
                ]
            })
        )
      | BodyArena.EApply (callee_id, arguments) ->
          let lower_argument (argument: BodyArena.apply_argument) =
            if argument.implicit then
              error
                (UnsupportedExpr {
                  expr_id;
                  reason = "implicit application arguments are not supported"
                })
            else
              match argument.label with
              | BodyArena.Positional -> lower_expr semantic_tree argument.value_id
              | label -> error
                (UnsupportedExpr {
                  expr_id;
                  reason = format
                    Format.[
                      str "non-positional application arguments are not supported: ";
                      str (label_to_string label);
                    ]
                })
          in
          (
            match SemanticTree.find_expr semantic_tree callee_id with
            | Some { desc=BodyArena.EVar constructor_path; _ } -> (
                match resolve_variant_constructor semantic_tree constructor_path with
                | ResolvedVariantConstructor (_layout, constructor) ->
                    let expected = constructor.payload_arity in
                    let actual = List.length arguments in
                    if not (Int.equal expected actual) then
                      error
                        (
                          UnsupportedExpr {
                            expr_id;
                            reason =
                              format
                                Format.[
                                  str "constructor `";
                                  str (SurfacePath.to_string constructor_path);
                                  str "` must be applied to exactly ";
                                  int expected;
                                  str " positional ";
                                  str
                                    (
                                      if Int.equal expected 1 then
                                        "argument"
                                      else
                                        "arguments"
                                    );
                                  str " in the current closed ordinary-variant lowering slice";
                                ];
                          }
                        )
                    else
                      Result.map
                        (fun payloads -> variant_constructor_expr constructor payloads)
                        (map_results arguments lower_argument)
                | MissingVariantConstructor ->
                    let arguments = map_results arguments lower_argument in
                    Result.and_then arguments (lower_direct_intrinsic_apply constructor_path)
                | AmbiguousVariantConstructor candidates ->
                    error
                      (UnsupportedExpr {
                        expr_id;
                        reason = format
                          Format.[
                            str "constructor `";
                            str (SurfacePath.to_string constructor_path);
                            str "` is ambiguous across visible ordinary variant declarations: ";
                            str (render_variant_constructor_names candidates);
                          ]
                      })
              )
            | Some _ ->
                let lower_callee callee_id =
                  match SemanticTree.find_expr semantic_tree callee_id with
                  | Some { desc=BodyArena.EVar path; _ } -> ok
                    (Core.Expr.Direct (unresolved_entity_id_of_typ_path path))
                  | Some _ -> Result.map
                    (fun callee -> Core.Expr.Indirect callee)
                    (lower_expr semantic_tree callee_id)
                  | None -> error (MissingExpr { expr_id = callee_id })
                in
                let arguments = map_results arguments lower_argument in
                let callee = lower_callee callee_id in
                validation_map2
                  callee
                  arguments
                  (fun callee arguments -> Core.Expr.Apply Core.Expr.{ callee; arguments })
            | None ->
                error (MissingExpr { expr_id = callee_id })
          )
      | BodyArena.ETuple element_ids ->
          Result.map
            (fun elements -> Core.Expr.Tuple elements)
            (map_results element_ids (lower_expr semantic_tree))
      | BodyArena.ELet (binding_ids, body_id) ->
          lower_local_let semantic_tree ~expr_id binding_ids body_id
      | BodyArena.ESequence element_ids ->
          lower_sequence_elements semantic_tree ~expr_id element_ids
      | BodyArena.EIf (condition_id, then_id, else_id) ->
          validation_map3
            (lower_expr semantic_tree condition_id)
            (lower_expr semantic_tree then_id)
            (lower_expr semantic_tree else_id)
            (fun condition then_ else_ ->
              Core.Expr.If_then_else Core.Expr.{ condition; then_; else_ })
      | BodyArena.EFun (parameters, body_id) ->
          Result.map
            (fun lambda -> Core.Expr.Lambda lambda)
            (lower_function semantic_tree ~name:(fresh_lambda_name expr_id) expr_id parameters body_id)
      | BodyArena.EMatch (scrutinee_id, cases) ->
          lower_closed_variant_match semantic_tree ~expr_id scrutinee_id cases
      | BodyArena.ERecord { base_id=None; fields } ->
          Result.and_then (resolve_record_construction_layout semantic_tree ~expr_id fields)
            (fun layout ->
              Result.and_then (lower_record_fields semantic_tree fields)
                (fun lowered_fields ->
                  Result.map (fun fields -> Core.Expr.Record fields)
                    (
                      map_results layout.labels
                        (fun label ->
                          match lowered_record_field lowered_fields label with
                          | Some value -> ok Core.Expr.{ label; value }
                          | None -> error
                            (UnsupportedExpr {
                              expr_id;
                              reason = format
                                Format.[
                                  str "record literal is missing field `";
                                  str label;
                                  str "` for record type ";
                                  str layout.type_name;
                                ]
                            }))
                    )))
      | BodyArena.ERecord { base_id=Some base_id; fields } ->
          Result.and_then (resolve_record_update_layout semantic_tree ~expr_id fields)
            (fun layout ->
              Result.and_then (validation_map2
                (lower_expr semantic_tree base_id)
                (lower_record_fields semantic_tree fields)
                (fun base lowered_fields -> (base, lowered_fields)))
                (fun (base, lowered_fields) ->
                  let base_name = fresh_record_base_name expr_id in
                  let base_entity_id = generated_expr_entity_id ~name:base_name expr_id 17 in
                  let base_var = Core.Expr.Var base_entity_id in
                  Result.map (fun fields ->
                    wrap_nonrecursive_let
                      Core.Expr.{ entity_id = base_entity_id; name = base_name; expr = base }
                      (Core.Expr.Record fields))
                    (
                      map_results (List.mapi (fun index label -> (index, label)) layout.labels)
                        (fun (index, label) ->
                          match lowered_record_field lowered_fields label with
                          | Some value -> ok Core.Expr.{ label; value }
                          | None -> ok
                            Core.Expr.{
                              label;
                              value = Core.Expr.Record_get Core.Expr.{
                                record = base_var;
                                label;
                                index
                              }
                            })
                    )))
      | BodyArena.EFieldAccess { receiver_id; label } ->
          Result.and_then (resolve_record_field_layout semantic_tree ~expr_id label)
            (fun layout ->
              match record_label_index layout label with
              | None -> error
                (UnsupportedExpr {
                  expr_id;
                  reason = format
                    Format.[
                      str "record field `";
                      str label;
                      str "` is not present in record type ";
                      str layout.type_name;
                    ]
                })
              | Some index -> Result.map
                (fun record -> Core.Expr.Record_get Core.Expr.{ record; label; index })
                (lower_expr semantic_tree receiver_id))
      | BodyArena.EChar value ->
          ok (Core.Expr.Constant (Core.Constant.Char value))
      | BodyArena.EArray _
      | BodyArena.EFor _
      | BodyArena.EWhile _
      | BodyArena.EFieldAssign _
      | BodyArena.EIndex _
      | BodyArena.ETry _
      | BodyArena.EPolyVariant _
      | BodyArena.ECoerce _
      | BodyArena.EModulePack _
      | BodyArena.ELocalModulePack _
      | BodyArena.ELocalModule _
      | BodyArena.ELocalOpen _
      | BodyArena.EUnsupported _
      | BodyArena.EHole _ ->
          error
            (UnsupportedExpr {
              expr_id;
              reason = "expression form is outside the first Typ -> Raml lowering slice"
            })
    )

and lower_record_fields = fun semantic_tree fields ->
  map_results fields
    (fun (field: BodyArena.record_expr_field) ->
      Result.map (fun value -> (field.label, value)) (lower_expr semantic_tree field.value_id))

and lower_local_binding_value = fun semantic_tree binding_id binding_name value_id ->
  match SemanticTree.find_expr semantic_tree value_id with
  | None -> error (MissingExpr { expr_id = value_id })
  | Some { desc=BodyArena.EFun (parameters, body_id); expr_id; _ } -> Result.map
    (fun lambda -> Core.Expr.Lambda lambda)
    (lower_function semantic_tree ~name:binding_name expr_id parameters body_id)
  | Some _ -> lower_expr semantic_tree value_id

and lower_local_binding = fun semantic_tree binding_id ->
  match SemanticTree.find_binding semantic_tree binding_id with
  | None -> error (MissingBinding { binding_id })
  | Some (binding: BodyArena.binding) ->
      if SurfacePath.is_empty binding.scope_path then
        let binding_name =
          match binding.name with
          | Some name -> ok name
          | None -> error
            (UnsupportedBinding {
              binding_id;
              reason = "local let bindings must introduce named variables"
            })
        in
        let pattern_name = lower_var_pattern semantic_tree binding.pattern_id in
        Result.and_then (validation_map2
          binding_name
          pattern_name
          (fun binding_name pattern_name -> (binding_name, pattern_name)))
          (fun (binding_name, pattern_name) ->
            if String.equal binding_name pattern_name then
              Result.map
                (fun expr ->
                  (
                    binding.recursive,
                    Core.Expr.{
                      entity_id = semantic_entity_id ~name:binding_name binding_id;
                      name = binding_name;
                      expr
                    }
                  ))
                (lower_local_binding_value semantic_tree binding_id binding_name binding.value_id)
            else
              error
                (UnsupportedBinding {
                  binding_id;
                  reason = "local let binding name and variable pattern must match"
                }))
      else
        error
          (UnsupportedBinding {
            binding_id;
            reason = "module-scoped local let bindings are not supported in the first Typ -> Raml lowering slice"
          })

and lower_single_pattern_local_let = fun semantic_tree binding_id body_id ->
  match SemanticTree.find_binding semantic_tree binding_id with
  | None -> error (MissingBinding { binding_id })
  | Some (binding: BodyArena.binding) ->
      if not (SurfacePath.is_empty binding.scope_path) then
        error
          (UnsupportedBinding {
            binding_id;
            reason = "module-scoped local let bindings are not supported in the first Typ -> Raml lowering slice"
          })
      else if binding.recursive then
        error
          (UnsupportedBinding {
            binding_id;
            reason = "non-variable local let patterns cannot be recursive in the current Typ -> Raml lowering slice"
          })
      else
        Result.and_then
          (validation_map2
            (lower_expr semantic_tree binding.value_id)
            (lower_expr semantic_tree body_id)
            (fun value body -> (value, body)))
          (fun (value, body) -> bind_pattern semantic_tree binding.pattern_id value body)

and lower_local_let_group = fun semantic_tree ~expr_id binding_ids body_id ->
  let bindings = map_results binding_ids (lower_local_binding semantic_tree) in
  Result.and_then bindings
    (fun bindings ->
      let any_recursive =
        List.fold_left
          (fun recursive (binding_is_recursive, _) -> recursive || binding_is_recursive)
          false
          bindings
      in
      let mixed_recursive_flags =
        List.exists
          (fun (binding_is_recursive, _) -> not (Bool.equal binding_is_recursive any_recursive))
          bindings
      in
      if mixed_recursive_flags then
        error
          (UnsupportedExpr {
            expr_id;
            reason = "mixed recursive and nonrecursive local let groups are not supported"
          })
      else
        Result.map
          (fun body ->
            Core.Expr.Let Core.Expr.{
              rec_flag =
                if any_recursive then
                  Recursive
                else
                  Nonrecursive;
              bindings = List.map snd bindings;
              body;
            })
          (lower_expr semantic_tree body_id))

and lower_local_let = fun semantic_tree ~expr_id binding_ids body_id ->
  match binding_ids with
  | [ binding_id ] -> (
      match SemanticTree.find_binding semantic_tree binding_id with
      | None -> error (MissingBinding { binding_id })
      | Some binding -> (
          match SemanticTree.find_pattern semantic_tree binding.pattern_id with
          | None -> error (MissingPattern { pattern_id = binding.pattern_id })
          | Some { desc=BodyArena.PVar _; _ } -> lower_local_let_group
            semantic_tree
            ~expr_id
            binding_ids
            body_id
          | Some _ -> lower_single_pattern_local_let semantic_tree binding_id body_id
        )
    )
  | _ -> lower_local_let_group semantic_tree ~expr_id binding_ids body_id

let lower_binding = fun semantic_tree binding_id ->
  match SemanticTree.find_binding semantic_tree binding_id with
  | None -> error (MissingBinding { binding_id })
  | Some (binding: BodyArena.binding) ->
      if SurfacePath.is_empty binding.scope_path then
        let name =
          match binding.name with
          | Some name -> ok name
          | None -> ok ""
        in
        let pattern_name = lower_top_level_pattern semantic_tree binding.pattern_id in
        let named_binding =
          Result.and_then (validation_map2
            name
            pattern_name
            (fun name pattern_name -> (name, pattern_name)))
            (fun (name, pattern_name) ->
              match pattern_name with
              | Named pattern_name ->
                  if String.equal name pattern_name then
                    ok (Some name)
                  else
                    error
                      (UnsupportedBinding {
                        binding_id;
                        reason = "binding name and variable pattern must match"
                      })
              | Unit ->
                  if String.equal name "" then
                    ok None
                  else
                    error
                      (UnsupportedBinding {
                        binding_id;
                        reason = "unit-pattern top-level bindings must stay anonymous"
                      }))
        in
        match SemanticTree.find_expr semantic_tree binding.value_id with
        | None -> error (MissingExpr { expr_id = binding.value_id })
        | Some { desc=BodyArena.EFun (parameters, body_id); expr_id; _ } ->
            Result.and_then named_binding
              (
                function
                | None -> error
                  (UnsupportedBinding {
                    binding_id;
                    reason = "unit-pattern top-level bindings cannot introduce functions"
                  })
                | Some name ->
                    Result.map
                      (fun lambda ->
                        let entity_id = semantic_entity_id ~name binding_id in
                        {
                          export = Some Core.Export.{ name; symbol = entity_id };
                          item = Core.Init_item.Binding Core.Binding.{
                            entity_id;
                            name;
                            expr = Core.Expr.Lambda lambda
                          }
                        })
                      (lower_function semantic_tree ~name expr_id parameters body_id)
              )
        | Some _ ->
            Result.and_then named_binding
              (
                function
                | Some name ->
                    Result.map
                      (fun expr ->
                        let entity_id = semantic_entity_id ~name binding_id in
                        {
                          export = Some Core.Export.{ name; symbol = entity_id };
                          item = Core.Init_item.Binding Core.Binding.{ entity_id; name; expr }
                        })
                      (lower_expr semantic_tree binding.value_id)
                | None -> Result.map
                  (fun expr -> { export = None; item = Core.Init_item.Eval expr })
                  (lower_expr semantic_tree binding.value_id)
              )
      else
        error
          (UnsupportedBinding {
            binding_id;
            reason = "nested-scope bindings are not supported in the first Typ -> Raml lowering slice"
          })

let lower_value_item = fun semantic_tree (value_item: ItemTree.value_item) ->
  let bindings = map_results value_item.binding_ids (lower_binding semantic_tree) in
  Result.map
    (fun bindings ->
      let exports =
        List.fold_right
          (fun (binding: lowered_binding) acc ->
            match binding.export with
            | Some export -> export :: acc
            | None -> acc)
          bindings
          []
      in
      let items = bindings |> List.map (fun (binding: lowered_binding) -> binding.item) in
      Core.Binding_group.{
        rec_flag =
          if value_item.recursive then
            Recursive
          else
            Nonrecursive;
        items;
        exports;
      })
    bindings

let lower_item = fun semantic_tree (item: ItemTree.item) ->
  match item with
  | ItemTree.Value value_item when SurfacePath.is_empty value_item.scope_path -> lower_value_item
    semantic_tree
    value_item
  | ItemTree.Value value_item -> error
    (UnsupportedItem {
      item_id = value_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ value_item.scope_path
    })
  | ItemTree.Type type_item -> error
    (UnsupportedItem {
      item_id = type_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ type_item.scope_path
    })
  | ItemTree.Exception exception_item -> error
    (UnsupportedItem {
      item_id = exception_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ exception_item.scope_path
    })
  | ItemTree.ExtensionConstructor extension_item -> error
    (UnsupportedItem {
      item_id = extension_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ extension_item.scope_path
    })
  | ItemTree.DeclaredValue declared_value_item -> error
    (UnsupportedItem {
      item_id = declared_value_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ declared_value_item.scope_path
    })
  | ItemTree.Open open_item -> error
    (UnsupportedItem {
      item_id = open_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ open_item.scope_path
    })
  | ItemTree.Include include_item -> error
    (UnsupportedItem {
      item_id = include_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ include_item.scope_path
    })
  | ItemTree.ModuleAlias module_alias_item -> error
    (UnsupportedItem {
      item_id = module_alias_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ module_alias_item.scope_path
    })
  | ItemTree.Unsupported unsupported_item -> error
    (UnsupportedItem {
      item_id = unsupported_item.item_id;
      kind = item_kind item;
      scope_path = core_surface_path_of_typ unsupported_item.scope_path
    })

let lower_file = fun ~(source_unit:Source_unit.t) (semantic_tree: SemanticTree.file) ->
  match source_unit.kind with
  | Source_unit.Interface -> error (UnsupportedSourceKind { kind = source_unit.kind })
  | Source_unit.Implementation ->
      let lowered_groups = map_results
        (ItemTree.items semantic_tree.item_tree |> List.filter keep_runtime_item)
        (lower_item semantic_tree)
      in
      Result.map
        (fun lowered_groups ->
          let exports =
            List.fold_right
              (fun (group: Core.Binding_group.t) acc -> group.exports @ acc)
              lowered_groups
              []
          in
          Core.Compilation_unit.{
            unit_id = Core.Unit_id.of_source_unit source_unit;
            exports;
            init = lowered_groups
          }
          |> reclassify_compilation_unit)
        lowered_groups
