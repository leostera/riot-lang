open Std
open Model
module Typ_diagnostic = Diagnostic
open Syn

type state = {
  source: Source.t;
  source_owner: string;
  mutable scope_path: IdentPath.t;
  mutable next_origin_id: int;
  mutable next_pattern_id: int;
  mutable next_expr_id: int;
  mutable next_binding_id: int;
  mutable next_item_id: int;
  mutable next_type_constructor_id: int;
  mutable next_wildcard_type_var_id: int;
  mutable next_lowered_type_var_id: int;
  mutable next_constructor_id: int;
  mutable next_label_id: int;
  mutable next_synthetic_name: int;
  mutable local_abstract_type_params: (string * int) list;
  mutable origins: OriginMap.origin list;
  mutable patterns: BodyArena.pattern_node list;
  mutable expressions: BodyArena.expr_node list;
  mutable bindings: BodyArena.binding list;
  mutable items: ItemTree.item list;
  mutable diagnostics: Typ_diagnostic.t list;
  mutable declared_type_names: (string * IdentPath.t * TypeConstructorId.t) list;
  mutable module_type_templates: (IdentPath.t * module_type_template) list;
  mutable local_module_aliases: (string * IdentPath.t) list;
  mutable local_module_binding_groups: (string * BodyArena.local_module_scope) list;
  mutable local_module_functors: (IdentPath.t * local_module_functor) list;
}

and module_type_template = {
  abstract_types: (string * TypeRepr.named_type_head) list;
  values: TypeRepr.package_value list;
}

and local_module_functor = {
  parameter_name: string;
  result_module_type: Cst.module_type option;
  body: Cst.module_expression;
}

let unresolved_type_parameter_hole_id = (-100)

let unsupported_core_type_hole_id = (-101)

let unsupported_record_constructor_payload_hole_id = (-102)

let fresh_wildcard_type_var_id = fun (state: state) ->
  let current = state.next_wildcard_type_var_id in
  state.next_wildcard_type_var_id <- current - 1;
  current

let fresh_lowered_type_var_id = fun (state: state) ->
  let current = state.next_lowered_type_var_id in
  state.next_lowered_type_var_id <- current - 1;
  current

let ident_path = fun path -> path |> Cst.Ident.segments |> List.map Cst.Token.text |> IdentPath.of_segments

let resolve_local_module_alias_path = fun (state: state) path ->
  match IdentPath.to_segments path with
  | head :: tail -> (
      match List.assoc_opt head state.local_module_aliases with
      | Some resolved_path -> tail |> List.fold_left IdentPath.append_name resolved_path
      | None -> path
    )
  | [] -> path

let path_text = fun path -> path |> ident_path |> IdentPath.to_string

let last_path_segment_text = fun path ->
  match List.rev (Cst.Ident.segments path) with
  | segment :: _ -> Cst.Token.text segment
  | [] -> ""

let qualify_scoped_name = fun scope_path name ->
  IdentPath.append_name scope_path name

let register_local_module_functor = fun (state: state) name template ->
  let path = qualify_scoped_name state.scope_path name in
  state.local_module_functors <- (path, template) :: state.local_module_functors

let resolve_local_module_functor = fun (state: state) path ->
  let lookup candidate_path =
    state.local_module_functors
    |> List.find_map
      (fun (template_path, template) ->
        if IdentPath.equal template_path candidate_path then
          Some template
        else
          None)
  in
  if IdentPath.is_bare path then
    match IdentPath.last_name path with
    | Some name -> IdentPath.prefixes state.scope_path
    |> List.rev
    |> List.find_map (fun scope_path -> lookup (qualify_scoped_name scope_path name))
    | None -> None
  else
    lookup path

let source_owner = fun (source: Source.t) ->
  match source.origin with
  | Source.Path path -> Path.remove_extension path |> Path.to_string
  | Source.Label label -> Path.v label |> Path.remove_extension |> Path.to_string

let resolve_named_type_name = fun (state: state) name ->
  let rec loop = function
    | [] -> None
    | scope_path :: rest ->
        if
          List.exists
            (fun (candidate_name, candidate_scope_path, _) ->
              String.equal candidate_name name && IdentPath.equal candidate_scope_path scope_path)
            state.declared_type_names
        then
          let type_constructor_id =
            state.declared_type_names
            |> List.find_map
              (fun (candidate_name, candidate_scope_path, candidate_id) ->
                if
                  String.equal candidate_name name && IdentPath.equal candidate_scope_path scope_path
                then
                  Some candidate_id
                else
                  None)
          in
          type_constructor_id
          |> Option.map
            (fun type_constructor_id ->
              TypeRepr.named_head ~type_constructor_id ~name:(qualify_scoped_name scope_path name))
        else
          loop rest
  in
  IdentPath.prefixes state.scope_path |> List.rev |> loop

let resolve_named_type_path = fun (state: state) path ->
  let external_head () = TypeRepr.named_head
    ~type_constructor_id:(TypeConstructorId.of_path path)
    ~name:path in
  match IdentPath.split_last path with
  | Some (scope_path, type_name) ->
      state.declared_type_names |> List.find_map
        (fun (candidate_name, candidate_scope_path, candidate_id) ->
          if
            String.equal candidate_name type_name && IdentPath.equal candidate_scope_path scope_path
          then
            Some (TypeRepr.named_head ~type_constructor_id:candidate_id ~name:path)
          else
            None) |> fun resolved ->
        Option.or_else resolved
          (fun () ->
            Option.or_else
              (BuiltinTypeConstructors.head_of_path path)
              (fun () -> Some (external_head ())))
  | None -> BuiltinTypeConstructors.head_of_path path

let register_module_type_template = fun (state: state) name template ->
  let path = qualify_scoped_name state.scope_path name in
  state.module_type_templates <- (path, template) :: state.module_type_templates

let resolve_module_type_template = fun (state: state) path ->
  let lookup candidate_path =
    state.module_type_templates
    |> List.find_map
      (fun (template_path, template) ->
        if IdentPath.equal template_path candidate_path then
          Some template
        else
          None)
  in
  if IdentPath.is_bare path then
    match IdentPath.last_name path with
    | Some name -> IdentPath.prefixes state.scope_path
    |> List.rev
    |> List.find_map (fun scope_path -> lookup (qualify_scoped_name scope_path name))
    | None -> None
  else
    lookup path

let register_declared_type_name = fun (state: state) name ->
  match
    state.declared_type_names |> List.find_map
      (fun (candidate_name, candidate_scope_path, candidate_id) ->
        if
          String.equal candidate_name name && IdentPath.equal candidate_scope_path state.scope_path
        then
          Some candidate_id
        else
          None)
  with
  | Some type_constructor_id -> type_constructor_id
  | None ->
      let type_constructor_id = TypeConstructorId.make ~owner:state.source_owner ~local_id:state.next_type_constructor_id in
      let () =
        state.next_type_constructor_id <- state.next_type_constructor_id + 1
      in
      let binding = (name, state.scope_path, type_constructor_id) in
      let () =
        state.declared_type_names <- binding :: state.declared_type_names
      in
      type_constructor_id

let with_nonrec_current_type_name_hidden = fun (state: state) (declaration: Cst.TypeDeclaration.t) f ->
  match Cst.TypeDeclaration.nonrec_token declaration with
  | None -> f ()
  | Some _ ->
      let type_name = Cst.TypeDeclaration.name_token declaration |> Cst.Token.text in
      let saved_declared_type_names = state.declared_type_names in
      let () =
        state.declared_type_names <- List.filter
          (fun (candidate_name, candidate_scope_path, _) ->
            not
              (String.equal candidate_name type_name && IdentPath.equal candidate_scope_path state.scope_path))
          state.declared_type_names
      in
      try
        let lowered = f () in
        let () =
          state.declared_type_names <- saved_declared_type_names
        in
        lowered
      with
      | error ->
          let () =
            state.declared_type_names <- saved_declared_type_names
          in
          raise error

let is_module_name = fun name ->
  String.length name > 0
  && Char.uppercase_ascii name.[0] = name.[0]
  && Char.lowercase_ascii name.[0] != name.[0]

let rec module_path_segments_of_expr = fun (state: state) ->
  function
  | Cst.Expression.Path { path; _ } ->
      let segments = Cst.Ident.segments path |> List.map Cst.Token.text in
      if List.is_empty segments || not (List.for_all is_module_name segments) then
        None
      else
        Some (resolve_local_module_alias_path state (IdentPath.of_segments segments))
  | Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match module_path_segments_of_expr state receiver with
      | Some path ->
          let field_name = Cst.Token.text field_name in
          if is_module_name field_name then
            Some (IdentPath.append_name path field_name)
          else
            None
      | None -> None
    )
  | _ ->
      None

let rec module_path_segments_of_module_expression = fun (state: state) ->
  function
  | Cst.ModuleExpression.Path path -> Some (resolve_local_module_alias_path state (ident_path path))
  | Cst.ModuleExpression.Parenthesized { inner; _ }
  | Cst.ModuleExpression.Attribute { module_expression=inner; _ }
  | Cst.ModuleExpression.Constraint { module_expression=inner; _ } -> module_path_segments_of_module_expression
    state
    inner
  | _ -> None

let rec unpacked_expression_of_module_expression = function
  | Cst.ModuleExpression.ModuleUnpack { expression; package_type; _ } -> Some (expression, package_type)
  | Cst.ModuleExpression.Parenthesized { inner; _ }
  | Cst.ModuleExpression.Attribute { module_expression=inner; _ }
  | Cst.ModuleExpression.Constraint { module_expression=inner; _ } -> unpacked_expression_of_module_expression
    inner
  | _ -> None

let rec structure_items_of_module_expression = function
  | Cst.ModuleExpression.Parenthesized { inner; _ }
  | Cst.ModuleExpression.Attribute { module_expression=inner; _ }
  | Cst.ModuleExpression.Constraint { module_expression=inner; _ } -> structure_items_of_module_expression
    inner
  | module_expression -> CstBuilder.structure_items_of_module_expression module_expression

let rec include_target_path_of_module_type = function
  | Cst.ModuleType.Path path -> Some (ident_path path)
  | Cst.ModuleType.TypeOf { module_path; _ } -> Some (ident_path module_path)
  | Cst.ModuleType.Parenthesized { inner; _ }
  | Cst.ModuleType.Attribute { module_type=inner; _ }
  | Cst.ModuleType.With { base=inner; _ } -> include_target_path_of_module_type inner
  | _ -> None

let binding_name_of_pattern =
  let operator_name operator_tokens = operator_tokens |> List.map Cst.Token.text |> String.concat "" in
  let rec loop = function
    | Cst.Pattern.Identifier { name_token; _ } -> Some (Cst.Token.text name_token)
    | Cst.Pattern.Operator { operator_tokens; _ } -> Some (operator_name operator_tokens)
    | Cst.Pattern.Parenthesized { inner; _ } -> loop inner
    | Cst.Pattern.Typed { pattern; _ } -> loop pattern
    | _ -> None
  in
  loop

let type_param_bindings = fun (declaration: Cst.TypeDeclaration.t) ->
  declaration |> Cst.TypeDeclaration.type_params |> List.filter_map
    (fun parameter ->
      match Cst.TypeParameter.type_variable parameter with
      | Some type_variable -> Some (Cst.TypeVariable.name type_variable)
      | None -> None) |> List.mapi (fun index name -> (name, index))

let type_parameter_bindings = fun parameters ->
  parameters |> List.filter_map
    (fun parameter ->
      match Cst.TypeParameter.type_variable parameter with
      | Some type_variable -> Some (Cst.TypeVariable.name type_variable)
      | None -> None) |> List.mapi (fun index name -> (name, index))

let builtin_type_of_name = fun name arguments ->
  match (name, arguments) with
  | ("int", []) -> Some TypeRepr.int
  | ("float", []) -> Some TypeRepr.float
  | ("bool", []) -> Some TypeRepr.bool
  | ("string", []) -> Some TypeRepr.string
  | ("char", []) -> Some TypeRepr.char
  | ("unit", []) -> Some TypeRepr.unit_
  | ("exn", []) -> Some (TypeRepr.named
    ~head:(TypeRepr.named_head
      ~type_constructor_id:BuiltinTypeConstructors.exn_type_constructor_id
      ~name:(IdentPath.of_name "exn"))
    ~arguments:[])
  | ("array", [ argument ]) -> Some (TypeRepr.array argument)
  | ("list", [ argument ]) -> Some (TypeRepr.list argument)
  | ("option", [ argument ]) -> Some (TypeRepr.option argument)
  | ("result", [ok_ty;error_ty]) -> Some (TypeRepr.result ok_ty error_ty)
  | ("seq", [ argument ]) -> Some (TypeRepr.seq argument)
  | _ -> None

let lower_arrow_label = fun (label: Cst.arrow_label option) ->
  match label with
  | None -> TypeRepr.Nolabel
  | Some (Cst.Named { label_token; _ }) -> TypeRepr.Labelled (Cst.Token.text label_token)
  | Some (Cst.OptionalNamed { label_token; _ }) -> TypeRepr.Optional (Cst.Token.text label_token)

let substitute_package_type =
  let rec loop replacements ty =
    let ty = TypeRepr.prune ty in
    match TypeRepr.view ty with
    | TypeRepr.Int
    | TypeRepr.Float
    | TypeRepr.Bool
    | TypeRepr.String
    | TypeRepr.Char
    | TypeRepr.Unit
    | TypeRepr.Hole _
    | TypeRepr.Var _ ->
        ty
    | TypeRepr.Option element ->
        let substituted_element = loop replacements element in
        if Std.Ptr.equal element substituted_element then
          ty
        else
          TypeRepr.option substituted_element
    | TypeRepr.Result (ok_ty, error_ty) ->
        let substituted_ok_ty = loop replacements ok_ty in
        let substituted_error_ty = loop replacements error_ty in
        if Std.Ptr.equal ok_ty substituted_ok_ty && Std.Ptr.equal error_ty substituted_error_ty then
          ty
        else
          TypeRepr.result substituted_ok_ty substituted_error_ty
    | TypeRepr.Array element ->
        let substituted_element = loop replacements element in
        if Std.Ptr.equal element substituted_element then
          ty
        else
          TypeRepr.array substituted_element
    | TypeRepr.List element ->
        let substituted_element = loop replacements element in
        if Std.Ptr.equal element substituted_element then
          ty
        else
          TypeRepr.list substituted_element
    | TypeRepr.Seq element ->
        let substituted_element = loop replacements element in
        if Std.Ptr.equal element substituted_element then
          ty
        else
          TypeRepr.seq substituted_element
    | TypeRepr.Named { head; arguments } ->
        let substituted_arguments = List.map (loop replacements) arguments in
        begin
          match Collections.HashMap.get replacements head.type_constructor_id with
          | Some replacement when List.is_empty substituted_arguments -> replacement
          | _ ->
              if List.for_all2 Std.Ptr.equal arguments substituted_arguments then
                ty
              else
                TypeRepr.named ~head ~arguments:substituted_arguments
        end
    | TypeRepr.Package signature ->
        let substituted_values =
          signature.values
          |> List.map
            (fun (value: TypeRepr.package_value) ->
              let substituted_scheme =
                TypeScheme.map_type_preserving (loop replacements) value.scheme
              in
              if Std.Ptr.equal value.scheme substituted_scheme then
                value
              else
                { value with scheme = substituted_scheme })
        in
        if List.for_all2 Std.Ptr.equal signature.values substituted_values then
          ty
        else
          TypeRepr.package ~values:substituted_values
    | TypeRepr.PolyVariant { bound; tags; inherited } ->
        let substituted_tags =
          List.map
            (fun (tag: TypeRepr.poly_variant_tag) ->
              match tag.payload_type with
              | Some payload_type ->
                  let substituted_payload_type = loop replacements payload_type in
                  if Std.Ptr.equal payload_type substituted_payload_type then
                    tag
                  else
                    { tag with payload_type = Some substituted_payload_type }
              | None -> tag)
            tags
        in
        let substituted_inherited = List.map (loop replacements) inherited in
        if
          List.for_all2 Std.Ptr.equal tags substituted_tags
          && List.for_all2 Std.Ptr.equal inherited substituted_inherited
        then
          ty
        else
          TypeRepr.poly_variant ~bound ~tags:substituted_tags ~inherited:substituted_inherited
    | TypeRepr.Tuple members ->
        let substituted_members = List.map (loop replacements) members in
        if List.for_all2 Std.Ptr.equal members substituted_members then
          ty
        else
          TypeRepr.tuple substituted_members
    | TypeRepr.Arrow { label; lhs; rhs } ->
        let substituted_lhs = loop replacements lhs in
        let substituted_rhs = loop replacements rhs in
        if Std.Ptr.equal lhs substituted_lhs && Std.Ptr.equal rhs substituted_rhs then
          ty
        else
          TypeRepr.arrow ~label ~lhs:substituted_lhs ~rhs:substituted_rhs
  in
  loop

let next_type_param_id = fun type_params ->
  type_params |> List.fold_left
    (fun current_max (_, id) ->
      Int.max current_max id)
    (-1) |> Int.succ

let fresh_type_params_from_binders = fun type_params binders ->
  let start = next_type_param_id type_params in
  binders |> List.mapi (fun offset binder -> (Cst.TypeBinder.name binder, start + offset))

let fresh_local_abstract_type_params_from_binders = fun (state: state) binders ->
  binders |> List.map (fun binder -> (Cst.TypeBinder.name binder, fresh_lowered_type_var_id state))

let rec lower_core_type = fun (state: state) type_params core_type ->
  let type_params = state.local_abstract_type_params @ type_params in
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ }
  | Cst.CoreType.Alias { type_=inner; _ } ->
      lower_core_type state type_params inner
  | Cst.CoreType.Poly { binders; body; _ } ->
      let params = fresh_type_params_from_binders type_params binders in
      lower_core_type state (params @ type_params) body
  | Cst.CoreType.Var { name_token; _ } -> (
      match List.assoc_opt (Cst.Token.text name_token) type_params with
      | Some id -> TypeRepr.make_var id
      | None -> TypeRepr.hole unresolved_type_parameter_hole_id
    )
  | Cst.CoreType.Wildcard _ ->
      TypeRepr.make_var (fresh_wildcard_type_var_id state)
  | Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let name = constructor_path |> ident_path |> resolve_local_module_alias_path state in
      let arguments = List.map (lower_core_type state type_params) arguments in
      begin
        match
          match (arguments, Cst.Ident.segments constructor_path) with
          | ([], [ segment ]) -> List.assoc_opt (Cst.Token.text segment) type_params
          | _ -> None
        with
        | Some id -> TypeRepr.make_var id
        | None -> (
            let resolved_head =
              match Cst.Ident.segments constructor_path with
              | [ segment ] -> resolve_named_type_name state (Cst.Token.text segment)
              | _ -> resolve_named_type_path state name
            in
            match resolved_head with
            | Some head -> TypeRepr.named ~head ~arguments
            | None -> (
                match builtin_type_of_name (IdentPath.to_string name) arguments with
                | Some builtin -> builtin
                | None -> TypeRepr.named_path ~name ~arguments
              )
          )
      end
  | Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      TypeRepr.arrow
        ~label:(lower_arrow_label label)
        ~lhs:(lower_core_type state type_params parameter_type)
        ~rhs:(lower_core_type state type_params result_type)
  | Cst.CoreType.Tuple { elements; _ } ->
      TypeRepr.tuple (List.map (lower_core_type state type_params) elements)
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      lower_package_type state type_params package_type
  | _ ->
      TypeRepr.hole unsupported_core_type_hole_id

and lower_package_type = fun (state: state) type_params (package_type: Cst.package_type) ->
  let module_type_path = ident_path package_type.module_type_path in
  match resolve_module_type_template state module_type_path with
  | Some template ->
      let abstract_head_for_constraint (constraint_: Cst.module_type_constraint) =
        match constraint_.constrained_type with
        | Cst.CoreType.Constr { constructor_path; arguments=[]; _ } ->
            let constrained_path = ident_path constructor_path in
            let constrained_name = IdentPath.last_name constrained_path in
            template.abstract_types |> List.find_map
              (fun ((type_name, (head: TypeRepr.named_type_head))) ->
                if IdentPath.equal head.name constrained_path then
                  Some head
                else
                  match constrained_name with
                  | Some constrained_name when String.equal constrained_name type_name -> Some head
                  | _ -> None)
        | _ -> None
      in
      let replacements = Collections.HashMap.with_capacity (List.length package_type.constraints) in
      let () =
        package_type.constraints
        |> List.iter
          (fun (constraint_: Cst.module_type_constraint) ->
            match abstract_head_for_constraint constraint_ with
            | Some head ->
                let replacement_type = lower_core_type state type_params constraint_.replacement_type in
                let _ = Collections.HashMap.insert replacements head.type_constructor_id replacement_type in
                ()
            | _ -> ())
      in
      let values = template.values
      |> List.map
        (fun (value: TypeRepr.package_value) -> {
          value
          with scheme = TypeScheme.map_type_preserving
            (substitute_package_type replacements)
            value.scheme;
        }) in
      TypeRepr.package ~values
  | None -> TypeRepr.hole unsupported_core_type_hole_id

let constructor_scheme = fun ~params ~result_type payload_type ->
  let body =
    match payload_type with
    | Some payload_type -> TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:payload_type ~rhs:result_type
    | None -> result_type
  in
  TypeScheme.of_explicit ~quantified:(List.map snd params) body

let invariant_param_variances = fun params ->
  List.map (fun _ -> TypeDecl.Invariant) params

let variant_constructor_payload = fun (state: state) type_params (
  constructor: Cst.VariantConstructor.t
) ->
  match Cst.VariantConstructor.result_type constructor with
  | Some _ -> (
      match Cst.VariantConstructor.arguments constructor with
      | Some (Cst.ConstructorArguments.Tuple [ Cst.CoreType.Record _ ]) ->
          Some (TypeRepr.hole unsupported_record_constructor_payload_hole_id)
      | Some (Cst.ConstructorArguments.Tuple members) ->
          let members = List.map (lower_core_type state type_params) members in
          begin
            match members with
            | [ member ] -> Some member
            | members -> Some (TypeRepr.tuple members)
          end
      | Some (Cst.ConstructorArguments.Record _) ->
          Some (TypeRepr.hole unsupported_record_constructor_payload_hole_id)
      | None ->
          None
    )
  | None -> (
      match Cst.VariantConstructor.arguments constructor with
      | Some (Cst.ConstructorArguments.Tuple [ Cst.CoreType.Record _ ]) ->
          Some (TypeRepr.hole unsupported_record_constructor_payload_hole_id)
      | Some (Cst.ConstructorArguments.Tuple members) ->
          let members = List.map (lower_core_type state type_params) members in
          begin
            match members with
            | [ member ] -> Some member
            | members -> Some (TypeRepr.tuple members)
          end
      | Some (Cst.ConstructorArguments.Record _) ->
          Some (TypeRepr.hole unsupported_record_constructor_payload_hole_id)
      | None -> (
          match Cst.VariantConstructor.payload_type constructor with
          | Some payload_type -> Some (lower_core_type state type_params payload_type)
          | None -> None
        )
    )

let make_state = fun source ->
  {
    source;
    source_owner = source_owner source;
    scope_path = IdentPath.empty;
    next_origin_id = 0;
    next_pattern_id = 0;
    next_expr_id = 0;
    next_binding_id = 0;
    next_item_id = 0;
    next_type_constructor_id = 0;
    next_wildcard_type_var_id = (-1);
    next_lowered_type_var_id = (-1_000);
    next_constructor_id = 0;
    next_label_id = 0;
    next_synthetic_name = 0;
    local_abstract_type_params = [];
    origins = [];
    patterns = [];
    expressions = [];
    bindings = [];
    items = [];
    diagnostics = [];
    declared_type_names = [];
    module_type_templates = [];
    local_module_aliases = [];
    local_module_binding_groups = [];
    local_module_functors = [];
  }

let add_diagnostic = fun (state: state) diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let add_origin = fun (state: state) ~semantic_id ~label syntax_node ->
  let origin_id = OriginId.of_int state.next_origin_id in
  let () =
    state.next_origin_id <- state.next_origin_id + 1
  in
  let origin = {
    OriginMap.origin_id;
    source_id = state.source.source_id;
    source_revision = state.source.revision;
    semantic_id;
    label;
    syntax_kind = Cst.syntax_kind syntax_node;
    span = Ceibo.Red.SyntaxNode.span syntax_node;
  }
  in
  let () =
    state.origins <- origin :: state.origins
  in
  origin_id

let add_pattern = fun (state: state) ~syntax_node ~label desc ->
  let pat_id = PatId.of_int state.next_pattern_id in
  let () =
    state.next_pattern_id <- state.next_pattern_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Pattern pat_id) ~label syntax_node in
  let node = { BodyArena.pat_id; origin_id; annotation = None; desc } in
  let () =
    state.patterns <- node :: state.patterns
  in
  pat_id

let annotate_pattern = fun (state: state) pat_id annotation ->
  let rec loop acc = function
    | [] -> List.rev acc
    | ((node: BodyArena.pattern_node) as current_node) :: rest ->
        if PatId.equal node.pat_id pat_id then
          List.rev_append acc ({ current_node with annotation = Some annotation } :: rest)
        else
          loop (current_node :: acc) rest
  in
  state.patterns <- loop [] state.patterns

let add_expr = fun (state: state) ~syntax_node ~label desc ->
  let expr_id = ExprId.of_int state.next_expr_id in
  let () =
    state.next_expr_id <- state.next_expr_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Expr expr_id) ~label syntax_node in
  let node = { BodyArena.expr_id; origin_id; desc } in
  let () =
    state.expressions <- node :: state.expressions
  in
  expr_id

let add_binding = fun (state: state) ~syntax_node ~name ~pattern_id ~annotation ~value_id ~recursive ->
  let binding_id = BindingId.of_int state.next_binding_id in
  let () =
    state.next_binding_id <- state.next_binding_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Binding binding_id) ~label:"binding" syntax_node in
  let binding = {
    BodyArena.binding_id;
    origin_id;
    scope_path = state.scope_path;
    name;
    pattern_id;
    annotation;
    value_id;
    recursive;
  }
  in
  let () =
    state.bindings <- binding :: state.bindings
  in
  binding_id

let add_item = fun (state: state) ~syntax_node item ->
  let item_id = ItemId.of_int state.next_item_id in
  let () =
    state.next_item_id <- state.next_item_id + 1
  in
  let origin_id = add_origin state ~semantic_id:(OriginMap.Item item_id) ~label:"item" syntax_node in
  let item =
    match item with
    | `Type declaration -> ItemTree.Type {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      declaration
    }
    | `Exception (exception_name, scheme) ->
        ItemTree.Exception {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          exception_name;
          scheme;
        }
    | `ExtensionConstructor (constructor_id, constructor_name, scheme, inline_record_labels) ->
        ItemTree.ExtensionConstructor {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          constructor_id;
          constructor_name;
          scheme;
          inline_record_labels;
        }
    | `Value (binding_ids, recursive) ->
        ItemTree.Value {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          binding_ids;
          recursive;
        }
    | `DeclaredValue (value_name, scheme) ->
        ItemTree.DeclaredValue {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          value_name;
          scheme;
        }
    | `Open module_path -> ItemTree.Open {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      module_path
    }
    | `Include module_path -> ItemTree.Include {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      module_path
    }
    | `ModuleAlias (alias_name, module_path) ->
        ItemTree.ModuleAlias {
          item_id;
          origin_id;
          scope_path = state.scope_path;
          alias_name;
          module_path;
        }
    | `Unsupported summary -> ItemTree.Unsupported {
      item_id;
      origin_id;
      scope_path = state.scope_path;
      summary
    }
  in
  let () =
    state.items <- item :: state.items
  in
  item

let rec lower_record_field_scheme = fun (state: state) type_params core_type ->
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ } ->
      lower_record_field_scheme state type_params inner
  | Cst.CoreType.Poly { binders; body; _ } ->
      let params = fresh_type_params_from_binders type_params binders in
      let lowered = lower_core_type state (params @ type_params) body in
      let quantified = TypeRepr.union (List.map snd params) (TypeRepr.free_vars lowered) in
      TypeScheme.of_explicit ~quantified lowered
  | _ ->
      TypeScheme.of_type (lower_core_type state type_params core_type)

let lower_record_label = fun (state: state) type_params (field: Cst.RecordField.t) ->
  {
    TypeDecl.label_id = LabelId.of_int state.next_label_id;
    TypeDecl.name = Cst.RecordField.name field;
    field_type = lower_record_field_scheme state type_params (Cst.RecordField.field_type field);
    mutable_ = Option.is_some (Cst.RecordField.mutable_token field)
  }
  |> fun label ->
    let () =
      state.next_label_id <- state.next_label_id + 1
    in
    label

let lower_record_type_label = fun (state: state) type_params (field: Cst.record_type_field) ->
  {
    TypeDecl.label_id = LabelId.of_int state.next_label_id;
    TypeDecl.name = Cst.Token.text field.field_name;
    field_type = lower_record_field_scheme state type_params field.field_type;
    mutable_ = Option.is_some field.mutable_token
  }
  |> fun label ->
    let () =
      state.next_label_id <- state.next_label_id + 1
    in
    label

let rec collect_core_type_var_names = fun core_type ->
  let append_unique acc name =
    if List.exists (String.equal name) acc then
      acc
    else
      acc @ [ name ]
  in
  let collect_many items =
    List.fold_left
      (fun acc item -> collect_core_type_var_names item |> List.fold_left append_unique acc)
      []
      items
  in
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ }
  | Cst.CoreType.Alias { type_=inner; _ } -> collect_core_type_var_names inner
  | Cst.CoreType.Var { name_token; _ } -> [ Cst.Token.text name_token ]
  | Cst.CoreType.Constr { arguments; _ } -> collect_many arguments
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } -> collect_many
    [ parameter_type; result_type ]
  | Cst.CoreType.Tuple { elements; _ } -> collect_many elements
  | Cst.CoreType.Record { fields; _ } -> fields
  |> List.fold_left
    (fun acc (field: Cst.record_type_field) ->
      collect_core_type_var_names field.field_type |> List.fold_left append_unique acc)
    []
  | _ -> []

let append_constructor_type_var_name = fun acc name ->
  if List.exists
      (fun (existing, _) ->
        String.equal existing name)
      acc then
    acc
  else
    acc @ [ (name, List.length acc) ]

let extend_constructor_type_params = fun params names ->
  List.fold_left append_constructor_type_var_name params names

let collect_record_type_field_var_names = fun fields ->
  fields |> List.fold_left
    (fun acc (field: Cst.record_type_field) ->
      collect_core_type_var_names field.field_type |> List.fold_left
        (fun acc name ->
          if List.exists (String.equal name) acc then
            acc
          else
            acc @ [ name ])
        acc)
    []

let inline_record_labels_for_constructor = fun (state: state) constructor_params (
  constructor: Cst.VariantConstructor.t
) ->
  match Cst.VariantConstructor.arguments constructor with
  | Some (Cst.ConstructorArguments.Record { fields; _ }) ->
      Some (List.map (lower_record_label state constructor_params) fields)
  | Some (Cst.ConstructorArguments.Tuple [ Cst.CoreType.Record { fields; _ } ]) ->
      Some (List.map (lower_record_type_label state constructor_params) fields)
  | _ -> (
      match Cst.VariantConstructor.payload_type constructor with
      | Some (Cst.CoreType.Record { fields; _ }) -> Some (List.map
        (lower_record_type_label state constructor_params)
        fields)
      | _ -> None
    )

let constructor_type_param_bindings = fun params (constructor: Cst.VariantConstructor.t) ->
  let params =
    match Cst.VariantConstructor.arguments constructor with
    | Some (Cst.ConstructorArguments.Tuple members) ->
        members
        |> List.fold_left
          (fun acc member -> extend_constructor_type_params acc (collect_core_type_var_names member))
          params
    | Some (Cst.ConstructorArguments.Record { fields; _ }) ->
        fields
        |> List.fold_left
          (fun acc field ->
            extend_constructor_type_params
              acc
              (collect_core_type_var_names (Cst.RecordField.field_type field)))
          params
    | None -> (
        match Cst.VariantConstructor.payload_type constructor with
        | Some (Cst.CoreType.Record { fields; _ }) -> collect_record_type_field_var_names fields
        |> List.fold_left append_constructor_type_var_name params
        | Some payload_type -> extend_constructor_type_params
          params
          (collect_core_type_var_names payload_type)
        | None -> params
      )
  in
  match Cst.VariantConstructor.result_type constructor with
  | Some result_type -> extend_constructor_type_params
    params
    (collect_core_type_var_names result_type)
  | None -> params

let lower_poly_variant_bound = function
  | Cst.PolyVariantBound.Exact -> TypeDecl.Exact
  | Cst.PolyVariantBound.UpperBound _ -> TypeDecl.UpperBound
  | Cst.PolyVariantBound.LowerBound _ -> TypeDecl.LowerBound

let lower_poly_variant_tag = fun (state: state) type_params (tag: Cst.PolyVariantTag.t) ->
  {
    TypeDecl.name = Cst.PolyVariantTag.name tag;
    payload_type = Cst.PolyVariantTag.payload_type tag
    |> Option.map (lower_core_type state type_params)
  }

let lower_poly_variant_manifest = fun (state: state) type_params (poly_variant: Cst.PolyVariant.t) ->
  let (tags, inherited) =
    Cst.PolyVariant.fields poly_variant
    |> List.fold_left
      (fun (tags, inherited) field ->
        match field with
        | Cst.RowField.Tag tag -> (lower_poly_variant_tag state type_params tag :: tags, inherited)
        | Cst.RowField.Inherit { type_; _ } -> (
          tags,
          lower_core_type state type_params type_ :: inherited
        ))
      ([], [])
  in
  TypeDecl.PolyVariant {
    bound = lower_poly_variant_bound (Cst.PolyVariant.kind poly_variant);
    tags = List.rev tags;
    inherited = List.rev inherited
  }

let add_unsupported_item = fun (state: state) ~context syntax_node ->
  let syntax_kind = Cst.syntax_kind syntax_node in
  let summary = SyntaxKind.to_string syntax_kind in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context;
        recovery = Typ_diagnostic.PlaceholderItem;
        reason = None;
      }
    )
  in
  let _ = add_item state ~syntax_node (`Unsupported summary) in
  ()

let add_unsupported_structure_item = fun (state: state) syntax_node ->
  add_unsupported_item state ~context:Typ_diagnostic.StructureItem syntax_node

let add_unsupported_signature_item = fun (state: state) syntax_node ->
  add_unsupported_item state ~context:Typ_diagnostic.SignatureItem syntax_node

let rec register_type_declaration_names = fun (state: state) (declaration: Cst.TypeDeclaration.t) ->
  let _ = register_declared_type_name
    state
    (Cst.Token.text (Cst.TypeDeclaration.name_token declaration)) in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> register_type_declaration_names state next
  | None -> ()

let lowered_type_declaration = fun (state: state) (declaration: Cst.TypeDeclaration.t) ->
  let syntax_node = Cst.TypeDeclaration.syntax_node declaration in
  let type_name = Cst.TypeDeclaration.name_token declaration |> Cst.Token.text in
  let nonrec_ = Option.is_some (Cst.TypeDeclaration.nonrec_token declaration) in
  let type_constructor_id = register_declared_type_name state type_name in
  let params = type_param_bindings declaration in
  let result_type = TypeRepr.named
    ~head:(TypeRepr.named_head
      ~type_constructor_id
      ~name:(qualify_scoped_name state.scope_path type_name))
    ~arguments:(params |> List.map (fun (_, id) -> TypeRepr.make_var id)) in
  let lowered_declaration =
    with_nonrec_current_type_name_hidden state declaration
      (fun () ->
        let lowered_manifest_alias = Cst.TypeDeclaration.manifest_alias declaration
        |> Option.map (fun manifest -> TypeDecl.Alias (lower_core_type state params manifest)) in
        match Cst.TypeDeclaration.type_definition declaration with
        | Cst.TypeDefinition.Abstract ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors = [];
              labels = [];
              manifest = lowered_manifest_alias;
            }
        | Cst.TypeDefinition.Alias { manifest; _ } ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors = [];
              labels = [];
              manifest = Some (TypeDecl.Alias (lower_core_type state params manifest));
            }
        | Cst.TypeDefinition.Variant { constructors; _ } ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors =
                constructors |> List.map
                  (fun (constructor: Cst.VariantConstructor.t) ->
                    let constructor_params = constructor_type_param_bindings params constructor in
                    let payload_type = variant_constructor_payload state constructor_params constructor in
                    let generalized =
                      Option.is_some (Cst.VariantConstructor.result_type constructor)
                    in
                    let result_type =
                      match Cst.VariantConstructor.result_type constructor with
                      | Some result_type -> lower_core_type state constructor_params result_type
                      | None -> result_type
                    in
                    let inline_record_labels = inline_record_labels_for_constructor
                      state
                      constructor_params
                      constructor in
                    {
                      TypeDecl.constructor_id = ConstructorId.of_int state.next_constructor_id;
                      TypeDecl.name = Cst.VariantConstructor.name constructor;
                      scheme = constructor_scheme ~params:constructor_params ~result_type payload_type;
                      generalized;
                      inline_record_labels
                    }
                    |> fun constructor ->
                      let () =
                        state.next_constructor_id <- state.next_constructor_id + 1
                      in
                      constructor);
              labels = [];
              manifest = lowered_manifest_alias;
            }
        | Cst.TypeDefinition.Record { fields; _ } ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors = [];
              labels = List.map (lower_record_label state params) fields;
              manifest = lowered_manifest_alias;
            }
        | Cst.TypeDefinition.PolyVariant poly_variant ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors = [];
              labels = [];
              manifest = Option.or_else
                lowered_manifest_alias
                (fun () -> Some (lower_poly_variant_manifest state params poly_variant));
            }
        | Cst.TypeDefinition.Extensible _ ->
            Some {
              TypeDecl.type_constructor_id;
              TypeDecl.type_name = type_name;
              nonrec_;
              param_ids = List.map snd params;
              param_variances = invariant_param_variances params;
              constructors = [];
              labels = [];
              manifest = lowered_manifest_alias;
            }
        | _ -> None)
  in
  match lowered_declaration with
  | Some lowered_declaration -> Some lowered_declaration
  | None ->
      let () = add_unsupported_structure_item state syntax_node in
      None

let lower_type_declaration = fun (state: state) (declaration: Cst.TypeDeclaration.t) ->
  match lowered_type_declaration state declaration with
  | Some lowered_declaration ->
      let _ = add_item state ~syntax_node:(Cst.TypeDeclaration.syntax_node declaration) (`Type lowered_declaration) in
      ()
  | None -> ()

let lower_exception_declaration = fun (state: state) (declaration: Cst.exception_declaration) ->
  let exception_name = Cst.Token.text declaration.name_token in
  let exn_type = TypeRepr.named
    ~head:(TypeRepr.named_head
      ~type_constructor_id:BuiltinTypeConstructors.exn_type_constructor_id
      ~name:(IdentPath.of_name "exn"))
    ~arguments:[] in
  let payload_type =
    match declaration.rhs with
    | Some (Cst.Payload { payload_type; _ }) -> Some (lower_core_type state [] payload_type)
    | Some (Cst.Alias _)
    | None -> None
  in
  let scheme =
    match payload_type with
    | Some payload_type -> TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:payload_type ~rhs:exn_type)
    | None -> TypeScheme.of_type exn_type
  in
  let _ = add_item state ~syntax_node:declaration.syntax_node (`Exception (exception_name, scheme)) in
  ()

let extension_target_result_type = fun (state: state) type_params (extension: Cst.TypeExtension.t) ->
  let owner_path = ident_path (Cst.TypeExtension.type_name extension) in
  let result_path =
    if IdentPath.is_bare owner_path && not (IdentPath.is_empty state.scope_path) then
      IdentPath.append_path state.scope_path owner_path
    else
      owner_path
  in
  let arguments =
    List.map (fun (_, id) -> TypeRepr.make_var id) type_params
  in
  TypeRepr.named_path ~name:result_path ~arguments

let lower_type_extension = fun (state: state) (extension: Cst.TypeExtension.t) ->
  let syntax_node = Cst.TypeExtension.syntax_node extension in
  let params = type_parameter_bindings (Cst.TypeExtension.type_params extension) in
  let default_result_type = extension_target_result_type state params extension in
  let () =
    Cst.TypeExtension.constructors extension
    |> List.iter
      (fun (constructor: Cst.VariantConstructor.t) ->
        let constructor_params = constructor_type_param_bindings params constructor in
        let payload_type = variant_constructor_payload state constructor_params constructor in
        let result_type =
          match Cst.VariantConstructor.result_type constructor with
          | Some result_type -> lower_core_type state constructor_params result_type
          | None -> default_result_type
        in
        let scheme = constructor_scheme ~params:constructor_params ~result_type payload_type in
        let constructor_id = ConstructorId.of_int state.next_constructor_id in
        let inline_record_labels = inline_record_labels_for_constructor state constructor_params constructor in
        let () =
          state.next_constructor_id <- state.next_constructor_id + 1
        in
        let _ = add_item
          state
          ~syntax_node
          (`ExtensionConstructor (
            constructor_id,
            Cst.VariantConstructor.name constructor,
            scheme,
            inline_record_labels
          )) in
        ())
  in
  ()

let declared_value_name = fun name_tokens ->
  let texts = List.map Cst.Token.text name_tokens in
  match texts with
  | "(" :: rest -> (
      match List.rev rest with
      | ")" :: inner_rev -> String.concat "" (List.rev inner_rev)
      | _ -> String.concat "" texts
    )
  | _ -> String.concat "" texts

let rec first_unsupported_core_type = fun core_type ->
  let find_many items = items |> List.find_map first_unsupported_core_type in
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ }
  | Cst.CoreType.Alias { type_=inner; _ } -> first_unsupported_core_type inner
  | Cst.CoreType.Var _ -> None
  | Cst.CoreType.Constr { arguments; _ } -> find_many arguments
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } -> Option.or_else
    (first_unsupported_core_type parameter_type)
    (fun () -> first_unsupported_core_type result_type)
  | Cst.CoreType.Tuple { elements; _ } -> find_many elements
  | Cst.CoreType.Wildcard _ -> None
  | Cst.CoreType.Class _
  | Cst.CoreType.Extension _
  | Cst.CoreType.Poly _
  | Cst.CoreType.PolyVariant _
  | Cst.CoreType.Record _
  | Cst.CoreType.Object _ -> Some core_type
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      package_type.constraints |> List.find_map
        (fun (constraint_: Cst.module_type_constraint) ->
          Option.or_else
            (first_unsupported_core_type constraint_.constrained_type)
            (fun () -> first_unsupported_core_type constraint_.replacement_type))

let add_unsupported_declared_core_type_diagnostic = fun (state: state) core_type ->
  match first_unsupported_core_type core_type with
  | None -> ()
  | Some unsupported_core_type ->
      let syntax_node = Cst.CoreType.syntax_node unsupported_core_type in
      add_diagnostic state
        (
          Typ_diagnostic.UnsupportedSyntax {
            syntax_span = Cst.token_body_span syntax_node;
            syntax_kind = Cst.syntax_kind syntax_node;
            context = Typ_diagnostic.SignatureItem;
            recovery = Typ_diagnostic.PlaceholderItem;
            reason = None;
          }
        )

let rec scheme_of_declared_core_type = fun (state: state) core_type ->
  match core_type with
  | Cst.CoreType.Parenthesized { inner; _ }
  | Cst.CoreType.Attribute { type_=inner; _ } ->
      scheme_of_declared_core_type state inner
  | Cst.CoreType.Poly { binders; body; _ } ->
      let params = fresh_type_params_from_binders [] binders in
      let ty = lower_core_type state params body in
      let quantified = TypeRepr.union (List.map snd params) (TypeRepr.free_vars ty) in
      TypeScheme.of_explicit ~quantified ty
  | _ ->
      let params = collect_core_type_var_names core_type
      |> List.mapi (fun index name -> (name, index)) in
      let ty = lower_core_type state params core_type in
      let quantified = TypeRepr.union (List.map snd params) (TypeRepr.free_vars ty) in
      TypeScheme.of_explicit ~quantified ty

let lower_module_type_template =
  let register_abstract_type_name state type_name =
    let type_constructor_id = TypeConstructorId.make ~owner:state.source_owner ~local_id:state.next_type_constructor_id in
    let () =
      state.next_type_constructor_id <- state.next_type_constructor_id + 1
    in
    let head = TypeRepr.named_head
      ~type_constructor_id
      ~name:(qualify_scoped_name state.scope_path type_name) in
    let binding = (type_name, state.scope_path, type_constructor_id) in
    let () =
      state.declared_type_names <- binding :: state.declared_type_names
    in
    (type_name, head)
  in
  let template_with_constraints state template constraints =
    let abstract_head_for_constraint (constraint_: Cst.module_type_constraint) =
      match constraint_.constrained_type with
      | Cst.CoreType.Constr { constructor_path; arguments=[]; _ } ->
          let constrained_path = ident_path constructor_path in
          let constrained_name = IdentPath.last_name constrained_path in
          template.abstract_types |> List.find_map
            (fun ((type_name, (head: TypeRepr.named_type_head))) ->
              if IdentPath.equal head.name constrained_path then
                Some head
              else
                match constrained_name with
                | Some constrained_name when String.equal constrained_name type_name -> Some head
                | _ -> None)
      | _ -> None
    in
    let replacements = Collections.HashMap.with_capacity (List.length constraints) in
    let () =
      constraints
      |> List.iter
        (fun (constraint_: Cst.module_type_constraint) ->
          match abstract_head_for_constraint constraint_ with
          | Some head ->
              let replacement_type = lower_core_type state [] constraint_.replacement_type in
              let _ = Collections.HashMap.insert replacements head.type_constructor_id replacement_type in
              ()
          | None -> ())
    in
    let values = template.values
    |> List.map
      (fun (value: TypeRepr.package_value) ->
        {
          value
          with scheme = TypeScheme.map_type_preserving
            (substitute_package_type replacements)
            value.scheme;
        }) in
    { template with values }
  in
  let rec loop state module_type =
    match module_type with
    | Cst.ModuleType.Signature _ -> (
        match CstBuilder.signature_items_of_module_type module_type with
        | Ok items ->
            let saved_declared_type_names = state.declared_type_names in
            let abstract_types =
              items
              |> List.concat_map
                (
                  function
                  | Cst.SignatureItem.TypeDeclaration declaration ->
                      let rec collect acc declaration =
                        let type_name = Cst.TypeDeclaration.name_token declaration |> Cst.Token.text in
                        let abstract_type = register_abstract_type_name state type_name in
                        match Cst.TypeDeclaration.next_and_declaration declaration with
                        | Some next -> collect (abstract_type :: acc) next
                        | None -> List.rev (abstract_type :: acc)
                      in
                      collect [] declaration
                  | _ -> []
                )
            in
            let values =
              items
              |> List.filter_map
                (
                  function
                  | Cst.SignatureItem.ValueDeclaration declaration ->
                      let value_name = declared_value_name
                        (Cst.ValueDeclaration.name_tokens declaration) in
                      Some (TypeRepr.package_value
                        ~name:value_name
                        ~scheme:(scheme_of_declared_core_type
                          state
                          (Cst.ValueDeclaration.type_ declaration)))
                  | Cst.SignatureItem.ExternalDeclaration declaration ->
                      let value_name = declared_value_name declaration.name_tokens in
                      Some (TypeRepr.package_value
                        ~name:value_name
                        ~scheme:(scheme_of_declared_core_type state declaration.type_))
                  | _ ->
                      None
                )
            in
            let () =
              state.declared_type_names <- saved_declared_type_names
            in
            Some { abstract_types; values }
        | Error builder_error ->
            let () = add_diagnostic state (Typ_diagnostic.CstBuilderError { builder_error }) in
            None
      )
    | Cst.ModuleType.Path path ->
        resolve_module_type_template state (ident_path path)
    | Cst.ModuleType.With { base; constraints; _ } ->
        loop state base
        |> Option.map (fun template -> template_with_constraints state template constraints)
    | Cst.ModuleType.Parenthesized { inner; _ }
    | Cst.ModuleType.Attribute { module_type=inner; _ } ->
        loop state inner
    | _ ->
        None
  in
  loop

let lower_module_type_ascription = fun (state: state) ~syntax_node module_type ->
  match lower_module_type_template state module_type with
  | Some template ->
      template.values |> List.iter
        (fun (value: TypeRepr.package_value) ->
          let _ = add_item state ~syntax_node (`DeclaredValue (value.name, value.scheme)) in
          ())
  | None -> ()

let lower_value_declaration = fun (state: state) syntax_node name_tokens type_ ->
  let value_name = declared_value_name name_tokens in
  let () = add_unsupported_declared_core_type_diagnostic state type_ in
  let scheme = scheme_of_declared_core_type state type_ in
  let _ = add_item state ~syntax_node (`DeclaredValue (value_name, scheme)) in
  ()

let fresh_synthetic_name = fun (state: state) prefix ->
  let name = "$" ^ prefix ^ Int.to_string state.next_synthetic_name in
  let () =
    state.next_synthetic_name <- state.next_synthetic_name + 1
  in
  name

let with_scope = fun (state: state) scope_path f ->
  let previous_scope_path = state.scope_path in
  let () =
    state.scope_path <- scope_path
  in
  let result = f () in
  let () =
    state.scope_path <- previous_scope_path
  in
  result

let with_local_module_alias = fun (state: state) ~module_name ~module_path f ->
  let previous_aliases = state.local_module_aliases in
  let () =
    state.local_module_aliases <- (module_name, module_path) :: state.local_module_aliases
  in
  try
    let result = f () in
    let () =
      state.local_module_aliases <- previous_aliases
    in
    result
  with
  | error ->
      let () =
        state.local_module_aliases <- previous_aliases
      in
      raise error

let with_local_abstract_type_params = fun (state: state) type_params f ->
  let previous_type_params = state.local_abstract_type_params in
  let () =
    state.local_abstract_type_params <- type_params @ state.local_abstract_type_params
  in
  try
    let result = f () in
    let () =
      state.local_abstract_type_params <- previous_type_params
    in
    result
  with
  | error ->
      let () =
        state.local_abstract_type_params <- previous_type_params
      in
      raise error

let with_local_module_binding_groups = fun (state: state) ~module_name ~local_scope f ->
  let previous_local_scopes = state.local_module_binding_groups in
  let () =
    state.local_module_binding_groups <- (module_name, local_scope) :: state.local_module_binding_groups
  in
  try
    let result = f () in
    let () =
      state.local_module_binding_groups <- previous_local_scopes
    in
    result
  with
  | error ->
      let () =
        state.local_module_binding_groups <- previous_local_scopes
      in
      raise error

let local_module_scope_for_path = fun (state: state) module_path ->
  match IdentPath.to_segments module_path with
  | [ module_name ] -> List.assoc_opt module_name state.local_module_binding_groups
  | _ -> None

let int_text = fun (integer: Cst.integer_constant) ->
  let sign =
    match integer.Cst.sign_token with
    | Some sign -> Cst.Token.text sign
    | None -> ""
  in
  sign ^ Cst.Token.text integer.literal_token

let float_text = fun (float_: Cst.float_constant) ->
  let sign =
    match float_.Cst.sign_token with
    | Some sign -> Cst.Token.text sign
    | None -> ""
  in
  sign ^ Cst.Token.text float_.literal_token

let unsupported_syntax_kind = fun syntax_node -> Cst.syntax_kind syntax_node

let operator_name = fun operator_tokens ->
  operator_tokens |> List.map Cst.Token.text |> String.concat ""

let prefix_operator_name = function
  | "-" -> "~-"
  | "+" -> "~+"
  | "-." -> "~-."
  | "+." -> "~+."
  | operator -> operator

let supported_literal_subset = [
  Typ_diagnostic.IntLiteral;
  Typ_diagnostic.FloatLiteral;
  Typ_diagnostic.BoolLiteral;
  Typ_diagnostic.StringLiteral;
  Typ_diagnostic.CharLiteral;
  Typ_diagnostic.UnitLiteral;
]

let lower_unsupported_pattern = fun (state: state) ?reason pattern syntax_kind ->
  let syntax_node = Cst.Pattern.syntax_node pattern in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Pattern;
        recovery = Typ_diagnostic.RecoveryPattern;
        reason;
      }
    )
  in
  add_pattern
    state
    ~syntax_node
    ~label:"unsupported_pattern"
    (BodyArena.PUnsupported (SyntaxKind.to_string syntax_kind))

let lower_unsupported_expr = fun (state: state) ?reason expr syntax_kind ->
  let syntax_node = Cst.Expression.syntax_node expr in
  let () = add_diagnostic state
    (
      Typ_diagnostic.UnsupportedSyntax {
        syntax_kind;
        syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
        context = Typ_diagnostic.Expression;
        recovery = Typ_diagnostic.HoleExpression;
        reason;
      }
    )
  in
  add_expr
    state
    ~syntax_node
    ~label:"unsupported_expression"
    (BodyArena.EHole (SyntaxKind.to_string syntax_kind))

let rec lower_pattern = fun (state: state) pattern ->
  match pattern with
  | Cst.Pattern.Identifier { syntax_node; name_token; _ } ->
      add_pattern
        state
        ~syntax_node
        ~label:"identifier_pattern"
        (BodyArena.PVar (Cst.Token.text name_token))
  | Cst.Pattern.Operator { syntax_node; operator_tokens; _ } ->
      add_pattern
        state
        ~syntax_node
        ~label:"operator_pattern"
        (BodyArena.PVar (operator_name operator_tokens))
  | Cst.Pattern.Wildcard { syntax_node; _ } ->
      add_pattern state ~syntax_node ~label:"wildcard_pattern" BodyArena.PWildcard
  | Cst.Pattern.Literal { syntax_node; literal; _ } -> (
      match literal with
      | Cst.PatternLiteral.Int integer -> add_pattern
        state
        ~syntax_node
        ~label:"int_pattern"
        (BodyArena.PInt (int_text integer))
      | Cst.PatternLiteral.Float float_ -> add_pattern
        state
        ~syntax_node
        ~label:"float_pattern"
        (BodyArena.PFloat (float_text float_))
      | Cst.PatternLiteral.Bool { value; _ } -> add_pattern
        state
        ~syntax_node
        ~label:"bool_pattern"
        (BodyArena.PBool value)
      | Cst.PatternLiteral.String string_ -> add_pattern
        state
        ~syntax_node
        ~label:"string_pattern"
        (BodyArena.PString string_.contents)
      | Cst.PatternLiteral.Char char_ -> add_pattern
        state
        ~syntax_node
        ~label:"char_pattern"
        (BodyArena.PChar char_.contents)
      | Cst.PatternLiteral.Unit _ -> add_pattern state ~syntax_node ~label:"unit_pattern" BodyArena.PUnit
    )
  | Cst.Pattern.Tuple { syntax_node; elements; _ } ->
      let element_ids = elements
      |> List.map (fun (element: Cst.tuple_pattern_element) -> lower_pattern state element.pattern) in
      add_pattern state ~syntax_node ~label:"tuple_pattern" (BodyArena.PTuple element_ids)
  | Cst.Pattern.Or { syntax_node; alternatives; _ } ->
      let alternative_ids = List.map (lower_pattern state) alternatives in
      add_pattern state ~syntax_node ~label:"or_pattern" (BodyArena.POr alternative_ids)
  | Cst.Pattern.Constructor { syntax_node; constructor_path; arguments; existentials; _ } ->
      let lowered_existentials =
        existentials
        |> Option.map
          (fun ({ Cst.binders; _ }: Cst.constructor_pattern_existentials) ->
            fresh_local_abstract_type_params_from_binders state binders)
        |> Option.unwrap_or ~default:[]
      in
      with_local_abstract_type_params state lowered_existentials
        (fun () ->
          let argument_ids = List.map (lower_pattern state) arguments in
          add_pattern
            state
            ~syntax_node
            ~label:"constructor_pattern"
            (BodyArena.PConstructor {
              constructor = ident_path constructor_path;
              arguments = argument_ids
            }))
  | Cst.Pattern.Record { syntax_node; fields; closedness; _ } ->
      let fields =
        fields
        |> List.map
          (fun (field: Cst.record_pattern_field) ->
            let pattern_id =
              match field.pattern with
              | Some pattern -> lower_pattern state pattern
              | None ->
                  let field_name = last_path_segment_text field.field_path in
                  add_pattern
                    state
                    ~syntax_node:field.syntax_node
                    ~label:"record_punned_field_pattern"
                    (BodyArena.PVar field_name)
            in
            (
              { BodyArena.label = path_text field.field_path; pattern_id }: BodyArena.record_pattern_field
            ))
      in
      add_pattern state ~syntax_node ~label:"record_pattern"
        (
          BodyArena.PRecord {
            fields;
            open_ =
              (
                match closedness with
                | Cst.Open _ -> true
                | Cst.Closed -> false
              );
          }
        )
  | Cst.Pattern.List { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_pattern state) elements in
      add_pattern state ~syntax_node ~label:"list_pattern" (BodyArena.PList element_ids)
  | Cst.Pattern.Cons { syntax_node; head; tail; _ } ->
      let head_id = lower_pattern state head in
      let tail_id = lower_pattern state tail in
      add_pattern
        state
        ~syntax_node
        ~label:"cons_pattern"
        (BodyArena.PConstructor {
          constructor = IdentPath.of_name "::";
          arguments = [ head_id; tail_id ]
        })
  | Cst.Pattern.Alias { syntax_node; pattern; name_token; _ } ->
      let pattern_id = lower_pattern state pattern in
      add_pattern
        state
        ~syntax_node
        ~label:"alias_pattern"
        (BodyArena.PAlias { pattern_id; alias = Cst.Token.text name_token })
  | Cst.Pattern.PolyVariant { syntax_node; tag_token; payload; _ } ->
      let payload = payload |> Option.map (lower_pattern state) in
      add_pattern
        state
        ~syntax_node
        ~label:"poly_variant_pattern"
        (BodyArena.PPolyVariant { tag = Cst.Token.text tag_token; payload })
  | Cst.Pattern.FirstClassModule { syntax_node; binding; package_type; _ } ->
      let module_name =
        match binding with
        | Cst.Named { name_token } -> Some (Cst.Token.text name_token)
        | Cst.Anonymous _ -> None
      in
      let package_type = package_type |> Option.map (lower_package_type state []) in
      add_pattern
        state
        ~syntax_node
        ~label:"first_class_module_pattern"
        (BodyArena.PFirstClassModule { module_name; package_type })
  | Cst.Pattern.Parenthesized { inner; _ } ->
      lower_pattern state inner
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      let pattern_id = lower_pattern state pattern in
      let annotation = lower_core_type state [] type_ in
      let () = annotate_pattern state pattern_id annotation in
      pattern_id
  | _ ->
      lower_unsupported_pattern
        state
        pattern
        (unsupported_syntax_kind (Cst.Pattern.syntax_node pattern))

let recovered_parameter_pattern = fun (state: state) syntax_node ~label parameter ->
  match Cst.Parameter.binding_pattern parameter with
  | Some pattern -> lower_pattern state pattern
  | None -> (
      match Cst.Parameter.name parameter with
      | Some name -> add_pattern state ~syntax_node ~label (BodyArena.PVar name)
      | None -> add_pattern state ~syntax_node ~label:"unsupported_parameter" BodyArena.PWildcard
    )

let positional_function_parameter = fun pattern_id ->
  (
    { BodyArena.label = BodyArena.Positional; pattern_id; default_value_id = None }: BodyArena.function_parameter
  )

let labeled_function_parameter = fun label pattern_id ->
  (
    { BodyArena.label = BodyArena.Labeled label; pattern_id; default_value_id = None }: BodyArena.function_parameter
  )

let optional_function_parameter = fun label ~default_value_id pattern_id ->
  (
    { BodyArena.label = BodyArena.Optional label; pattern_id; default_value_id }: BodyArena.function_parameter
  )

let rec lower_parameter = fun (state: state) parameter ->
  match parameter with
  | Cst.Parameter.Positional { pattern; _ } ->
      positional_function_parameter (lower_pattern state pattern)
  | Cst.Parameter.Labeled labeled ->
      let pattern_id = recovered_parameter_pattern
        state
        labeled.syntax_node
        ~label:"labeled_parameter_pattern"
        parameter in
      labeled_function_parameter (Cst.Token.text labeled.label_token) pattern_id
  | Cst.Parameter.Optional optional ->
      let pattern_id = recovered_parameter_pattern
        state
        optional.syntax_node
        ~label:"optional_parameter_pattern"
        parameter in
      let default_value_id = optional.default_value |> Option.map (lower_expr state) in
      optional_function_parameter (Cst.Token.text optional.label_token) ~default_value_id pattern_id
  | parameter ->
      let syntax_node = Cst.Parameter.syntax_node parameter in
      let () = add_diagnostic
        state
        (Typ_diagnostic.ParameterLoweredAsPositional {
          parameter_span = Ceibo.Red.SyntaxNode.span syntax_node
        }) in
      positional_function_parameter
        (recovered_parameter_pattern state syntax_node ~label:"recovered_parameter" parameter)

and synthetic_var_pattern = fun (state: state) syntax_node ~label ->
  let name = fresh_synthetic_name state label in
  let pat_id = add_pattern
    state
    ~syntax_node
    ~label:("synthetic_" ^ label ^ "_pattern")
    (BodyArena.PVar name) in
  (name, pat_id)

and lower_match_cases = fun (state: state) cases ->
  List.map
    (fun (case: Cst.match_case) ->
      let pattern_id = lower_pattern state case.pattern in
      let guard_id = case.guard |> Option.map (lower_expr state) in
      let body_id = lower_expr state case.body in
      { BodyArena.pattern_id; guard_id; body_id })
    cases

and lower_function_like = fun (state: state) ~syntax_node ~parameters ~body_annotation_type ~body ->
  let rec split_parameters local_type_params runtime_parameters = function
    | Cst.Parameter.LocallyAbstract parameter :: rest ->
        let new_params = fresh_local_abstract_type_params_from_binders state parameter.binders in
        split_parameters (new_params @ local_type_params) runtime_parameters rest
    | parameter :: rest ->
        split_parameters local_type_params (parameter :: runtime_parameters) rest
    | [] ->
        (List.rev local_type_params, List.rev runtime_parameters)
  in
  let (local_type_params, runtime_parameters) = split_parameters [] [] parameters in
  with_local_abstract_type_params state local_type_params
    (fun () ->
      let parameter_ids = List.map (lower_parameter state) runtime_parameters in
      let body_annotation = body_annotation_type |> Option.map (lower_core_type state []) in
      let wrap_body_annotation body_id =
        match body_annotation with
        | Some target_type -> add_expr
          state
          ~syntax_node
          ~label:"annotated_function_body"
          (BodyArena.ECoerce { value_id = body_id; target_type })
        | None -> body_id
      in
      let body_id =
        match body with
        | `Expr expression -> lower_expr state expression |> wrap_body_annotation
        | `Cases cases ->
            let (synthetic_name, synthetic_pattern_id) = synthetic_var_pattern
              state
              syntax_node
              ~label:"function_arg" in
            let argument_expr_id = add_expr
              state
              ~syntax_node
              ~label:"synthetic_function_argument"
              (BodyArena.EVar (IdentPath.of_name synthetic_name)) in
            let match_id = add_expr
              state
              ~syntax_node
              ~label:"function_match_body"
              (BodyArena.EMatch (argument_expr_id, lower_match_cases state cases)) in
            let match_id = wrap_body_annotation match_id in
            let parameter_ids = parameter_ids
            @ [ positional_function_parameter synthetic_pattern_id ] in
            add_expr
              state
              ~syntax_node
              ~label:"wrapped_fun"
              (BodyArena.EFun (parameter_ids, match_id))
      in
      match body with
      | `Expr _ -> add_expr
        state
        ~syntax_node
        ~label:"fun_expression"
        (BodyArena.EFun (parameter_ids, body_id))
      | `Cases _ -> body_id)

and lower_binding_source = fun (state: state) ~syntax_node ~binding_pattern ~parameters ~value ~recursive ->
  let rec explicit_scheme_of_core_type core_type =
    match core_type with
    | Cst.CoreType.Parenthesized { inner; _ }
    | Cst.CoreType.Attribute { type_=inner; _ } ->
        explicit_scheme_of_core_type inner
    | Cst.CoreType.Poly { binders; body; _ } ->
        let params = fresh_type_params_from_binders [] binders in
        let lowered = lower_core_type state params body in
        let quantified = TypeRepr.union (List.map snd params) (TypeRepr.free_vars lowered) in
        TypeScheme.of_explicit ~quantified lowered
    | core_type ->
        let params = collect_core_type_var_names core_type
        |> List.mapi (fun index name -> (name, index)) in
        let lowered = lower_core_type state params core_type in
        let quantified = TypeRepr.union (List.map snd params) (TypeRepr.free_vars lowered) in
        TypeScheme.of_explicit ~quantified lowered
  in
  let rec peel_binding_annotation ~parameters = function
    | Cst.Expression.Parenthesized { inner; _ } -> peel_binding_annotation ~parameters inner
    | Cst.Expression.Polymorphic { expression; type_; _ } ->
        if List.is_empty parameters then
          (expression, Some (explicit_scheme_of_core_type type_), None)
        else
          (expression, None, None)
    | Cst.Expression.TypeAscription { expression; kind=Cst.Type { type_; _ }; _ } ->
        if List.is_empty parameters then
          (expression, Some (explicit_scheme_of_core_type type_), None)
        else
          (expression, None, Some type_)
    | expression -> (expression, None, None)
  in
  let pattern_id = lower_pattern state binding_pattern in
  let (value, annotation, body_annotation) = peel_binding_annotation ~parameters value in
  let value_id =
    match parameters with
    | [] -> lower_expr state value
    | _ -> lower_function_like
      state
      ~syntax_node
      ~parameters
      ~body_annotation_type:body_annotation
      ~body:(`Expr value)
  in
  let name = binding_name_of_pattern binding_pattern in
  add_binding state ~syntax_node ~name ~pattern_id ~annotation ~value_id ~recursive

and binding_ids_of_let_binding_group = fun (state: state) let_binding ->
  let recursive = Cst.LetBinding.is_recursive let_binding in
  let binding_ids = let_binding :: Cst.LetBinding.and_bindings let_binding
  |> List.map
    (fun (binding: Cst.let_binding) ->
      lower_binding_source
        state
        ~syntax_node:(Cst.LetBinding.syntax_node binding)
        ~binding_pattern:(Cst.LetBinding.binding_pattern binding)
        ~parameters:(Cst.LetBinding.parameters binding)
        ~value:(Cst.LetBinding.value binding)
        ~recursive) in
  binding_ids

and lower_let_binding_group = fun (state: state) let_binding ->
  let recursive = Cst.LetBinding.is_recursive let_binding in
  let binding_ids = binding_ids_of_let_binding_group state let_binding in
  add_item
    state
    ~syntax_node:(Cst.LetBinding.syntax_node let_binding)
    (`Value (binding_ids, recursive))

and lower_let_expression_bindings = fun (state: state) (let_expression: Cst.let_expression) ->
  let recursive = Option.is_some let_expression.rec_token in
  let head = lower_binding_source
    state
    ~syntax_node:let_expression.syntax_node
    ~binding_pattern:let_expression.binding_pattern
    ~parameters:let_expression.parameters
    ~value:let_expression.bound_value
    ~recursive in
  let tail =
    match let_expression.and_binding with
    | None -> []
    | Some binding -> Cst.LetBinding.and_bindings binding
    |> fun rest ->
      binding :: rest
      |> List.map
        (fun (binding: Cst.let_binding) ->
          lower_binding_source
            state
            ~syntax_node:(Cst.LetBinding.syntax_node binding)
            ~binding_pattern:(Cst.LetBinding.binding_pattern binding)
            ~parameters:(Cst.LetBinding.parameters binding)
            ~value:(Cst.LetBinding.value binding)
            ~recursive)
  in
  head :: tail

and lower_local_module_scope = fun (state: state) ~module_name module_expression ->
  let with_local_scope f =
    let scope_path = IdentPath.append_name state.scope_path module_name in
    let previous_scope_path = state.scope_path in
    let previous_declared_type_names = state.declared_type_names in
    let previous_module_type_templates = state.module_type_templates in
    let () =
      state.scope_path <- scope_path
    in
    try
      let result = f () in
      let () =
        state.scope_path <- previous_scope_path;
        state.declared_type_names <- previous_declared_type_names;
        state.module_type_templates <- previous_module_type_templates
      in
      result
    with
    | error ->
        let () =
          state.scope_path <- previous_scope_path;
          state.declared_type_names <- previous_declared_type_names;
          state.module_type_templates <- previous_module_type_templates
        in
        raise error
  in
  let rec loop = function
    | Cst.ModuleExpression.Parenthesized { inner; _ }
    | Cst.ModuleExpression.Attribute { module_expression=inner; _ }
    | Cst.ModuleExpression.Constraint { module_expression=inner; _ } -> loop inner
    | Cst.ModuleExpression.Path _ -> None
    | module_expression ->
        with_local_scope
          (fun () ->
            match CstBuilder.structure_items_of_module_expression module_expression with
            | Error builder_error ->
                let () = add_diagnostic state (Typ_diagnostic.CstBuilderError { builder_error }) in
                None
            | Ok items ->
                let binding_groups, type_decls =
                  items
                  |> List.fold_left
                    (fun (binding_groups, type_decls) item ->
                      match item with
                      | Cst.StructureItem.Comment _
                      | Cst.StructureItem.Docstring _ ->
                          (binding_groups, type_decls)
                      | Cst.StructureItem.LetBinding binding ->
                          (
                            binding_groups @ [
                              { BodyArena.binding_ids = binding_ids_of_let_binding_group state binding }
                            ],
                            type_decls
                          )
                      | Cst.StructureItem.TypeDeclaration declaration ->
                          let () = register_type_declaration_names state declaration in
                          let rec collect declaration acc =
                            let acc =
                              match lowered_type_declaration state declaration with
                              | Some lowered_declaration ->
                                  acc @ [
                                    {
                                      FileSummary.scope_path = IdentPath.empty;
                                      declaration = lowered_declaration;
                                    }
                                  ]
                              | None -> acc
                            in
                            match Cst.TypeDeclaration.next_and_declaration declaration with
                            | Some next -> collect next acc
                            | None -> acc
                          in
                          (binding_groups, type_decls @ collect declaration [])
                      | unsupported_item ->
                          let () = add_unsupported_structure_item
                            state
                            (Cst.StructureItem.syntax_node unsupported_item) in
                          (binding_groups, type_decls))
                    ([], [])
                in
                Some { BodyArena.binding_groups; type_decls })
  in
  loop module_expression

and lower_apply = fun (state: state) expression ->
  let lower_argument = function
    | Cst.Positional argument ->
        (
          {
            BodyArena.label = BodyArena.Positional;
            implicit = false;
            value_id = lower_expr state argument
          }:
            BodyArena.apply_argument
        )
    | Cst.Labeled { syntax_node; label_token; value; _ } ->
        let value_id =
          match value with
          | Some value -> lower_expr state value
          | None -> add_expr
            state
            ~syntax_node
            ~label:"implicit_labeled_argument"
            (BodyArena.EVar (IdentPath.of_name (Cst.Token.text label_token)))
        in
        {
          BodyArena.label = BodyArena.Labeled (Cst.Token.text label_token);
          implicit = Option.is_none value;
          value_id
        }
    | Cst.Optional { syntax_node; label_token; value; _ } ->
        let value_id =
          match value with
          | Some value -> lower_expr state value
          | None -> add_expr
            state
            ~syntax_node
            ~label:"implicit_optional_argument"
            (BodyArena.EVar (IdentPath.of_name (Cst.Token.text label_token)))
        in
        {
          BodyArena.label = BodyArena.Optional (Cst.Token.text label_token);
          implicit = Option.is_none value;
          value_id
        }
  in
  let rec collect = function
    | Cst.Expression.Apply { callee; argument; _ } ->
        let (callee_id, arguments) = collect callee in
        (callee_id, arguments @ [ lower_argument argument ])
    | callee ->
        let callee_id = lower_expr state callee in
        (callee_id, [])
  in
  let syntax_node = Cst.Expression.syntax_node expression in
  let (callee_id, arguments) = collect expression in
  add_expr state ~syntax_node ~label:"apply_expression" (BodyArena.EApply (callee_id, arguments))

and lower_infix = fun (state: state) (infix: Cst.infix_expression) ->
  let syntax_node = infix.syntax_node in
  let operator_name = Cst.InfixExpression.operator infix in
  let operator_id = add_expr
    state
    ~syntax_node
    ~label:"infix_operator"
    (BodyArena.EVar (IdentPath.of_name operator_name)) in
  let left_id = lower_expr state infix.left in
  let right_id = lower_expr state infix.right in
  add_expr
    state
    ~syntax_node
    ~label:"infix_expression"
    (BodyArena.EApply (
      operator_id,
      [
        { BodyArena.label = BodyArena.Positional; implicit = false; value_id = left_id };
        { BodyArena.label = BodyArena.Positional; implicit = false; value_id = right_id };
      ]
    ))

and lower_list_expression = fun (state: state) (list_expression: Cst.list_expression) ->
  let nil_id = add_expr
    state
    ~syntax_node:list_expression.syntax_node
    ~label:"list_nil_expression"
    (BodyArena.EVar (IdentPath.of_name "[]")) in
  list_expression.elements |> List.rev |> List.fold_left
    (fun tail_id element ->
      let cons_id = add_expr
        state
        ~syntax_node:list_expression.syntax_node
        ~label:"list_cons_expression"
        (BodyArena.EVar (IdentPath.of_name "::")) in
      let head_id = lower_expr state element in
      add_expr
        state
        ~syntax_node:list_expression.syntax_node
        ~label:"list_literal_apply"
        (BodyArena.EApply (
          cons_id,
          [
            { BodyArena.label = BodyArena.Positional; implicit = false; value_id = head_id };
            { BodyArena.label = BodyArena.Positional; implicit = false; value_id = tail_id };
          ]
        )))
    nil_id

and lower_binding_operator_name = fun (binding: Cst.binding_operator_binding) ->
  Cst.Token.text binding.keyword_token ^ Cst.Token.text binding.operator_token

and lower_let_operator_expression = fun (state: state) (let_operator: Cst.let_operator_expression) ->
  match let_operator.binding.and_binding with
  | Some _ ->
      let syntax_node = let_operator.syntax_node in
      let () = add_diagnostic state
        (
          Typ_diagnostic.UnsupportedSyntax {
            syntax_kind = unsupported_syntax_kind syntax_node;
            syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
            context = Typ_diagnostic.Expression;
            recovery = Typ_diagnostic.HoleExpression;
            reason = None;
          }
        )
      in
      add_expr
        state
        ~syntax_node
        ~label:"unsupported_let_operator_expression"
        (BodyArena.EHole (SyntaxKind.to_string (unsupported_syntax_kind syntax_node)))
  | None ->
      let binding = let_operator.binding in
      let syntax_node = let_operator.syntax_node in
      let operator_id = add_expr
        state
        ~syntax_node
        ~label:"binding_operator_expression"
        (BodyArena.EVar (IdentPath.of_name (lower_binding_operator_name binding))) in
      let bound_value_id = lower_expr state binding.bound_value in
      let parameter_id = lower_pattern state binding.binding_pattern in
      let body_id = lower_expr state let_operator.body in
      let body_fun_id = add_expr
        state
        ~syntax_node
        ~label:"binding_operator_body"
        (BodyArena.EFun ([ positional_function_parameter parameter_id ], body_id)) in
      add_expr
        state
        ~syntax_node
        ~label:"let_operator_expression"
        (BodyArena.EApply (
          operator_id,
          [
            { BodyArena.label = BodyArena.Positional; implicit = false; value_id = bound_value_id };
            { BodyArena.label = BodyArena.Positional; implicit = false; value_id = body_fun_id };
          ]
        ))

and lower_expr = fun (state: state) expression ->
  match expression with
  | Cst.Expression.Path { syntax_node; path; _ } ->
      add_expr state ~syntax_node ~label:"path_expression" (BodyArena.EVar (ident_path path))
  | Cst.Expression.Constructor { syntax_node; constructor_path; payload; _ } -> (
      let constructor_name = ident_path constructor_path in
      match payload with
      | None -> add_expr
        state
        ~syntax_node
        ~label:"constructor_expression"
        (BodyArena.EVar constructor_name)
      | Some payload ->
          let callee_id = add_expr
            state
            ~syntax_node:(Cst.Ident.syntax_node constructor_path)
            ~label:"constructor_path_expression"
            (BodyArena.EVar constructor_name) in
          let payload_id = lower_expr state payload in
          add_expr
            state
            ~syntax_node
            ~label:"constructor_apply_expression"
            (BodyArena.EApply (
              callee_id,
              [ { BodyArena.label = BodyArena.Positional; implicit = false; value_id = payload_id } ]
            ))
    )
  | Cst.Expression.FieldAccess { syntax_node; receiver; field_name; _ } -> (
      match module_path_segments_of_expr state receiver with
      | Some module_path ->
          let qualified_name = IdentPath.append_name module_path (Cst.Token.text field_name) in
          add_expr
            state
            ~syntax_node
            ~label:"qualified_path_expression"
            (BodyArena.EVar qualified_name)
      | None ->
          let receiver_id = lower_expr state receiver in
          add_expr
            state
            ~syntax_node
            ~label:"field_access_expression"
            (BodyArena.EFieldAccess { receiver_id; label = Cst.Token.text field_name })
    )
  | Cst.Expression.Record (Cst.RecordExpression.Literal { syntax_node; fields; _ }) ->
      let fields = fields
      |> List.map
        (fun (field: Cst.record_expression_field) ->
          (
            { BodyArena.label = path_text field.field_path; value_id = lower_expr state field.value }:
              BodyArena.record_expr_field
          )) in
      add_expr
        state
        ~syntax_node
        ~label:"record_expression"
        (BodyArena.ERecord { base_id = None; fields })
  | Cst.Expression.Record (Cst.RecordExpression.Update { syntax_node; base; fields; _ }) ->
      let base_id = lower_expr state base in
      let fields = fields
      |> List.map
        (fun (field: Cst.record_expression_field) ->
          (
            { BodyArena.label = path_text field.field_path; value_id = lower_expr state field.value }:
              BodyArena.record_expr_field
          )) in
      add_expr
        state
        ~syntax_node
        ~label:"record_update_expression"
        (BodyArena.ERecord { base_id = Some base_id; fields })
  | Cst.Expression.FieldAssign { syntax_node; target; value; _ } ->
      let receiver_id = lower_expr state target.receiver in
      let value_id = lower_expr state value in
      add_expr
        state
        ~syntax_node
        ~label:"field_assign_expression"
        (BodyArena.EFieldAssign { receiver_id; label = Cst.Token.text target.field_name; value_id })
  | Cst.Expression.Operator { syntax_node; operator_tokens; _ } ->
      let operator = operator_tokens |> List.map Cst.Token.text |> String.concat "" in
      add_expr
        state
        ~syntax_node
        ~label:"operator_expression"
        (BodyArena.EVar (IdentPath.of_name operator))
  | Cst.Expression.Literal literal -> (
      match literal with
      | Cst.Literal.Int integer -> add_expr
        state
        ~syntax_node:integer.syntax_node
        ~label:"int_literal"
        (BodyArena.EInt (int_text integer))
      | Cst.Literal.Float float_ -> add_expr
        state
        ~syntax_node:float_.syntax_node
        ~label:"float_literal"
        (BodyArena.EFloat (float_text float_))
      | Cst.Literal.Bool { syntax_node; value; _ } -> add_expr
        state
        ~syntax_node
        ~label:"bool_literal"
        (BodyArena.EBool value)
      | Cst.Literal.String string_ -> add_expr
        state
        ~syntax_node:string_.syntax_node
        ~label:"string_literal"
        (BodyArena.EString string_.contents)
      | Cst.Literal.Char char_ -> add_expr
        state
        ~syntax_node:char_.syntax_node
        ~label:"char_literal"
        (BodyArena.EChar char_.contents)
      | Cst.Literal.Unit { syntax_node; _ } -> add_expr
        state
        ~syntax_node
        ~label:"unit_literal"
        BodyArena.EUnit
    )
  | Cst.Expression.Tuple { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_expr state) elements in
      add_expr state ~syntax_node ~label:"tuple_expression" (BodyArena.ETuple element_ids)
  | Cst.Expression.List list_expression ->
      lower_list_expression state list_expression
  | Cst.Expression.Array { syntax_node; elements; _ } ->
      let element_ids = List.map (lower_expr state) elements in
      add_expr state ~syntax_node ~label:"array_expression" (BodyArena.EArray element_ids)
  | Cst.Expression.Sequence { syntax_node; expressions; _ } ->
      let element_ids = List.map (lower_expr state) expressions in
      add_expr state ~syntax_node ~label:"sequence_expression" (BodyArena.ESequence element_ids)
  | Cst.Expression.ModulePack { syntax_node; module_expression; package_type; _ } -> (
      match module_path_segments_of_module_expression state module_expression with
      | Some module_path -> (
          match local_module_scope_for_path state module_path with
          | Some local_scope -> add_expr
            state
            ~syntax_node
            ~label:"local_module_pack_expression"
            (BodyArena.ELocalModulePack {
              local_scope;
              package_type = package_type |> Option.map (lower_package_type state [])
            })
          | None -> add_expr
            state
            ~syntax_node
            ~label:"module_pack_expression"
            (BodyArena.EModulePack {
              module_path;
              package_type = package_type |> Option.map (lower_package_type state [])
            })
        )
      | None ->
          let () = add_diagnostic state
            (
              Typ_diagnostic.UnsupportedSyntax {
                syntax_kind = unsupported_syntax_kind syntax_node;
                syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
                context = Typ_diagnostic.Expression;
                recovery = Typ_diagnostic.HoleExpression;
                reason = None;
              }
            )
          in
          add_expr
            state
            ~syntax_node
            ~label:"unsupported_module_pack_expression"
            (BodyArena.EHole (SyntaxKind.to_string (unsupported_syntax_kind syntax_node)))
    )
  | Cst.Expression.While while_expression ->
      let condition_id = lower_expr state while_expression.condition in
      let body_id = lower_expr state while_expression.body in
      add_expr state ~syntax_node:while_expression.syntax_node ~label:"while_expression"
        (BodyArena.EWhile { condition_id; body_id })
  | Cst.Expression.For for_expression ->
      let iterator_pattern_id = add_pattern
        state
        ~syntax_node:for_expression.syntax_node
        ~label:"for_iterator_pattern"
        (BodyArena.PVar (Cst.Token.text for_expression.iterator_token)) in
      let start_id = lower_expr state for_expression.start_expr in
      let end_id = lower_expr state for_expression.end_expr in
      let body_id = lower_expr state for_expression.body in
      let descending =
        match for_expression.direction with
        | Cst.Downto _ -> true
        | Cst.To _ -> false
      in
      add_expr state ~syntax_node:for_expression.syntax_node ~label:"for_expression"
        (
          BodyArena.EFor {
            iterator_pattern_id;
            descending;
            start_id;
            end_id;
            body_id;
          }
        )
  | Cst.Expression.Parenthesized { inner; _ } ->
      lower_expr state inner
  | Cst.Expression.TypeAscription { syntax_node; expression; kind; _ } -> (
      match kind with
      | Cst.Coerce { type_; _ } ->
          let value_id = lower_expr state expression in
          add_expr
            state
            ~syntax_node
            ~label:"coercion_expression"
            (BodyArena.ECoerce { value_id; target_type = lower_core_type state [] type_ })
      | Cst.Type _
      | Cst.ConstraintCoerce _ -> lower_expr state expression
    )
  | Cst.Expression.Polymorphic { syntax_node; expression; _ } ->
      let () = add_diagnostic
        state
        (Typ_diagnostic.IgnoredPolymorphicAnnotation {
          annotation_span = Ceibo.Red.SyntaxNode.span syntax_node
        }) in
      lower_expr state expression
  | Cst.Expression.Fun { syntax_node; parameters; body; _ } -> (
      match body with
      | Cst.Expression body -> lower_function_like
        state
        ~syntax_node
        ~parameters
        ~body_annotation_type:None
        ~body:(`Expr body)
      | Cst.Cases body -> lower_function_like
        state
        ~syntax_node
        ~parameters
        ~body_annotation_type:None
        ~body:(`Cases body.cases)
    )
  | Cst.Expression.Function { syntax_node; cases; _ } ->
      lower_function_like
        state
        ~syntax_node
        ~parameters:[]
        ~body_annotation_type:None
        ~body:(`Cases cases)
  | Cst.Expression.Apply _ ->
      lower_apply state expression
  | Cst.Expression.Index { syntax_node; collection; index; _ } ->
      let collection_id = lower_expr state collection in
      let index_id = lower_expr state index in
      add_expr
        state
        ~syntax_node
        ~label:"index_expression"
        (BodyArena.EIndex (collection_id, index_id))
  | Cst.Expression.Infix infix ->
      lower_infix state infix
  | Cst.Expression.If {
    syntax_node;
    condition;
    then_branch;
    else_branch;
    _
  } ->
      let condition_id = lower_expr state condition in
      let then_id = lower_expr state then_branch in
      let else_id =
        match else_branch with
        | Some else_branch -> lower_expr state else_branch
        | None -> add_expr state ~syntax_node ~label:"implicit_else_unit" BodyArena.EUnit
      in
      add_expr
        state
        ~syntax_node
        ~label:"if_expression"
        (BodyArena.EIf (condition_id, then_id, else_id))
  | Cst.Expression.Let let_expression ->
      let binding_ids = lower_let_expression_bindings state let_expression in
      let body_id = lower_expr state let_expression.body in
      add_expr
        state
        ~syntax_node:let_expression.syntax_node
        ~label:"let_expression"
        (BodyArena.ELet (binding_ids, body_id))
  | Cst.Expression.LetModule {
    syntax_node;
    module_name_token;
    module_expression;
    body;
    _
  } ->
      let source_module_name = Cst.Token.text module_name_token in
      begin
        match unpacked_expression_of_module_expression module_expression with
        | Some (expression, package_type) ->
            let pattern_id = add_pattern
              state
              ~syntax_node
              ~label:"let_module_unpack_pattern"
              (BodyArena.PFirstClassModule {
                module_name = Some source_module_name;
                package_type = package_type |> Option.map (lower_package_type state [])
              }) in
            let binding_id = add_binding
              state
              ~syntax_node
              ~name:None
              ~pattern_id
              ~annotation:None
              ~value_id:(lower_expr state expression)
              ~recursive:false in
            let body_id = lower_expr state body in
            add_expr
              state
              ~syntax_node
              ~label:"let_module_unpack_expression"
              (BodyArena.ELet ([ binding_id ], body_id))
        | None -> (
            match module_path_segments_of_module_expression state module_expression with
            | Some module_path -> with_local_module_alias
              state
              ~module_name:source_module_name
              ~module_path
              (fun () -> lower_expr state body)
            | None -> (
                match lower_local_module_scope state ~module_name:source_module_name module_expression with
                | Some local_scope ->
                    let body_id =
                      with_local_module_binding_groups
                        state
                        ~module_name:source_module_name
                        ~local_scope
                        (fun () -> lower_expr state body)
                    in
                    add_expr
                      state
                      ~syntax_node
                      ~label:"local_module_expression"
                      (BodyArena.ELocalModule {
                        module_name = source_module_name;
                        local_scope;
                        body_id
                      })
                | None ->
                    let () = add_diagnostic state
                      (
                        Typ_diagnostic.UnsupportedSyntax {
                          syntax_kind = unsupported_syntax_kind syntax_node;
                          syntax_span = Ceibo.Red.SyntaxNode.span syntax_node;
                          context = Typ_diagnostic.Expression;
                          recovery = Typ_diagnostic.HoleExpression;
                          reason = None;
                        }
                      )
                    in
                    add_expr
                      state
                      ~syntax_node
                      ~label:"unsupported_let_module_expression"
                      (BodyArena.EHole (SyntaxKind.to_string (unsupported_syntax_kind syntax_node)))
              )
          )
      end
  | Cst.Expression.LetOperator let_operator ->
      lower_let_operator_expression state let_operator
  | Cst.Expression.Match { syntax_node; scrutinee; cases; _ } ->
      let scrutinee_id = lower_expr state scrutinee in
      let cases = lower_match_cases state cases in
      add_expr state ~syntax_node ~label:"match_expression" (BodyArena.EMatch (scrutinee_id, cases))
  | Cst.Expression.Try { syntax_node; body; cases; _ } ->
      let body_id = lower_expr state body in
      let cases = lower_match_cases state cases in
      add_expr state ~syntax_node ~label:"try_expression" (BodyArena.ETry (body_id, cases))
  | Cst.Expression.PolyVariant { syntax_node; tag_token; payload; _ } ->
      let payload = payload |> Option.map (lower_expr state) in
      add_expr
        state
        ~syntax_node
        ~label:"poly_variant_expression"
        (BodyArena.EPolyVariant { tag = Cst.Token.text tag_token; payload })
  | Cst.Expression.LocalOpen (LetOpen { syntax_node; module_path; body; _ })
  | Cst.Expression.LocalOpen (Delimited { syntax_node; module_path; body; _ }) ->
      let body_id = lower_expr state body in
      add_expr
        state
        ~syntax_node
        ~label:"local_open_expression"
        (BodyArena.ELocalOpen { module_path = ident_path module_path; body_id })
  | Cst.Expression.Prefix { syntax_node; operator_token; operand; _ } -> (
      match (Cst.Token.text operator_token, operand) with
      | ("-", Cst.Expression.Literal (Cst.Literal.Int integer)) ->
          add_expr
            state
            ~syntax_node
            ~label:"negative_int_literal"
            (BodyArena.EInt ("-" ^ int_text integer))
      | ("-", Cst.Expression.Literal (Cst.Literal.Float float_)) ->
          add_expr
            state
            ~syntax_node
            ~label:"negative_float_literal"
            (BodyArena.EFloat ("-" ^ float_text float_))
      | _ ->
          let operator_id = add_expr
            state
            ~syntax_node
            ~label:"prefix_operator"
            (BodyArena.EVar (IdentPath.of_name (prefix_operator_name (Cst.Token.text operator_token)))) in
          let operand_id = lower_expr state operand in
          add_expr
            state
            ~syntax_node
            ~label:"prefix_expression"
            (BodyArena.EApply (
              operator_id,
              [ { BodyArena.label = BodyArena.Positional; implicit = false; value_id = operand_id } ]
            ))
    )
  | _ ->
      lower_unsupported_expr
        state
        expression
        (unsupported_syntax_kind (Cst.Expression.syntax_node expression))

let lower_top_level_expression = fun (state: state) expression ->
  let syntax_node = Cst.Expression.syntax_node expression in
  let pattern_id = add_pattern state ~syntax_node ~label:"top_level_expression_pattern" BodyArena.PWildcard in
  let value_id = lower_expr state expression in
  let binding_id = add_binding
    state
    ~syntax_node
    ~name:None
    ~pattern_id
    ~annotation:None
    ~value_id
    ~recursive:false in
  add_item state ~syntax_node (`Value ([ binding_id ], false))

let rec lower_structure_item = fun (state: state) item ->
  match item with
  | Cst.StructureItem.Comment _
  | Cst.StructureItem.Docstring _ ->
      ()
  | Cst.StructureItem.LetBinding binding ->
      let _ = lower_let_binding_group state binding in
      ()
  | Cst.StructureItem.Expression expression ->
      let _ = lower_top_level_expression state expression in
      ()
  | Cst.StructureItem.OpenStatement open_statement -> (
      match Cst.OpenStatement.module_path open_statement with
      | Some module_path ->
          let _ = add_item
            state
            ~syntax_node:(Cst.OpenStatement.syntax_node open_statement)
            (`Open (ident_path module_path)) in
          ()
      | None -> ()
    )
  | Cst.StructureItem.IncludeStatement include_statement ->
      lower_include_statement state include_statement
  | Cst.StructureItem.TypeDeclaration declaration ->
      let () = register_type_declaration_names state declaration in
      let rec loop declaration =
        let () = lower_type_declaration state declaration in
        match Cst.TypeDeclaration.next_and_declaration declaration with
        | Some declaration -> loop declaration
        | None -> ()
      in
      loop declaration
  | Cst.StructureItem.TypeExtension extension ->
      lower_type_extension state extension
  | Cst.StructureItem.ExternalDeclaration declaration ->
      lower_value_declaration state declaration.syntax_node declaration.name_tokens declaration.type_
  | Cst.StructureItem.ExceptionDeclaration declaration ->
      lower_exception_declaration state declaration
  | Cst.StructureItem.ModuleDeclaration declaration ->
      lower_module_declaration state declaration
  | Cst.StructureItem.ModuleTypeDeclaration declaration -> (
      match Cst.ModuleTypeDeclaration.module_type declaration with
      | Some module_type ->
          let module_type_name = Cst.ModuleTypeDeclaration.name declaration in
          begin
            match lower_module_type_template state module_type with
            | Some template -> register_module_type_template state module_type_name template
            | None -> add_unsupported_structure_item
              state
              (Cst.ModuleTypeDeclaration.syntax_node declaration)
          end
      | None -> add_unsupported_structure_item
        state
        (Cst.ModuleTypeDeclaration.syntax_node declaration)
    )
  | item ->
      add_unsupported_structure_item state (Cst.StructureItem.syntax_node item)

and lower_signature_items_of_module_type = fun (state: state) module_type ~on_unsupported ->
  match module_type with
  | Cst.ModuleType.Signature _ -> (
      match CstBuilder.signature_items_of_module_type module_type with
      | Ok items ->
          let _ = List.map (lower_signature_item state) items in
          ()
      | Error builder_error -> add_diagnostic
        state
        (Typ_diagnostic.CstBuilderError { builder_error })
    )
  | Cst.ModuleType.Parenthesized { inner; _ }
  | Cst.ModuleType.Attribute { module_type=inner; _ } ->
      lower_signature_items_of_module_type state inner ~on_unsupported
  | Cst.ModuleType.Path _
  | Cst.ModuleType.TypeOf _
  | Cst.ModuleType.Functor _
  | Cst.ModuleType.With _
  | Cst.ModuleType.Extension _ ->
      on_unsupported ()

and lower_signature_item = fun (state: state) item ->
  match item with
  | Cst.SignatureItem.Comment _
  | Cst.SignatureItem.Docstring _ ->
      ()
  | Cst.SignatureItem.ValueDeclaration declaration ->
      lower_value_declaration
        state
        (Cst.ValueDeclaration.syntax_node declaration)
        (Cst.ValueDeclaration.name_tokens declaration)
        (Cst.ValueDeclaration.type_ declaration)
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      lower_value_declaration state declaration.syntax_node declaration.name_tokens declaration.type_
  | Cst.SignatureItem.TypeDeclaration declaration ->
      let () = register_type_declaration_names state declaration in
      let rec loop declaration =
        let () = lower_type_declaration state declaration in
        match Cst.TypeDeclaration.next_and_declaration declaration with
        | Some declaration -> loop declaration
        | None -> ()
      in
      loop declaration
  | Cst.SignatureItem.TypeExtension extension ->
      lower_type_extension state extension
  | Cst.SignatureItem.ExceptionDeclaration declaration ->
      lower_exception_declaration state declaration
  | Cst.SignatureItem.OpenStatement open_statement -> (
      match Cst.OpenStatement.module_path open_statement with
      | Some module_path ->
          let _ = add_item
            state
            ~syntax_node:(Cst.OpenStatement.syntax_node open_statement)
            (`Open (ident_path module_path)) in
          ()
      | None -> ()
    )
  | Cst.SignatureItem.IncludeStatement include_statement ->
      lower_signature_include_statement state include_statement
  | Cst.SignatureItem.ModuleDeclaration declaration ->
      lower_module_signature_declaration state declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration -> (
      match Cst.ModuleTypeDeclaration.module_type declaration with
      | Some module_type ->
          let module_type_name = Cst.ModuleTypeDeclaration.name declaration in
          begin
            match lower_module_type_template state module_type with
            | Some template -> register_module_type_template state module_type_name template
            | None -> add_unsupported_signature_item
              state
              (Cst.ModuleTypeDeclaration.syntax_node declaration)
          end
      | None -> add_unsupported_signature_item
        state
        (Cst.ModuleTypeDeclaration.syntax_node declaration)
    )
  | item ->
      add_unsupported_signature_item state (Cst.SignatureItem.syntax_node item)

and lower_include_statement = fun (state: state) (include_statement: Cst.include_statement) ->
  let syntax_node = include_statement.syntax_node in
  match include_statement.target with
  | Cst.ModuleExpression module_expression -> (
      match module_path_segments_of_module_expression state module_expression with
      | Some path when IdentPath.is_empty path ->
          ()
      | Some module_path ->
          let _ = add_item state ~syntax_node (`Include module_path) in
          ()
      | None ->
          add_unsupported_structure_item state syntax_node
    )
  | Cst.ModuleType _ -> ()

and lower_signature_include_statement = fun (state: state) (include_statement: Cst.include_statement) ->
  let syntax_node = include_statement.syntax_node in
  match include_statement.target with
  | Cst.ModuleType module_type -> (
      match include_target_path_of_module_type module_type with
      | Some module_path ->
          let _ = add_item state ~syntax_node (`Include module_path) in
          ()
      | None -> lower_signature_items_of_module_type
        state
        module_type
        ~on_unsupported:(fun () -> add_unsupported_signature_item state syntax_node)
    )
  | Cst.ModuleExpression _ -> add_unsupported_signature_item state syntax_node

and lower_module_binding = fun (state: state) ~syntax_node ~module_name ~module_type ~module_expression ->
  let lower_module_items_under_scope ~nested_scope_path ~result_module_type module_expression =
    with_scope state nested_scope_path
      (fun () ->
        match structure_items_of_module_expression module_expression with
        | Ok items ->
            let _ = List.map (lower_structure_item state) items in
            let () =
              match result_module_type with
              | Some module_type -> lower_module_type_ascription state ~syntax_node module_type
              | None -> ()
            in
            ()
        | Error builder_error -> add_diagnostic
          state
          (Typ_diagnostic.CstBuilderError { builder_error }))
  in
  let lower_functor_application ~nested_scope_path template argument_path =
    with_local_module_alias
      state
      ~module_name:template.parameter_name
      ~module_path:argument_path
      (fun () ->
        lower_module_items_under_scope
          ~nested_scope_path
          ~result_module_type:template.result_module_type
          template.body)
  in
  match module_expression with
  | Cst.ModuleExpression.Apply { callee; argument; _ } -> (
      match module_path_segments_of_module_expression state callee, module_path_segments_of_module_expression
        state
        argument with
      | Some callee_path, Some argument_path -> (
          match resolve_local_module_functor state callee_path with
          | Some template ->
              let nested_scope_path = IdentPath.append_name state.scope_path module_name in
              lower_functor_application ~nested_scope_path template argument_path
          | None -> add_unsupported_structure_item state syntax_node
        )
      | _ -> add_unsupported_structure_item state syntax_node
    )
  | _ -> (
      match module_path_segments_of_module_expression state module_expression with
      | Some path when IdentPath.is_empty path ->
          ()
      | Some module_path ->
          let _ = add_item state ~syntax_node (`ModuleAlias (module_name, module_path)) in
          ()
      | None ->
          let nested_scope_path = IdentPath.append_name state.scope_path module_name in
          lower_module_items_under_scope ~nested_scope_path ~result_module_type:module_type module_expression
    )

and lower_module_declaration = fun (state: state) (declaration: Cst.ModuleStructure.t) ->
  let syntax_node = Cst.ModuleStructure.syntax_node declaration in
  if
    Cst.ModuleStructure.is_recursive declaration
    || Option.is_some (Cst.ModuleStructure.next_and_declaration declaration)
  then
    add_unsupported_structure_item state syntax_node
  else if not (List.is_empty (Cst.ModuleStructure.functor_parameters declaration)) then
    match Cst.ModuleStructure.functor_parameters declaration with
    | [ parameter ] -> register_local_module_functor
      state
      (Cst.ModuleStructure.name declaration)
      {
        parameter_name = Cst.Token.text parameter.name_token;
        result_module_type = Cst.ModuleStructure.module_type declaration;
        body = Cst.ModuleStructure.module_expression declaration
      }
    | _ -> add_unsupported_structure_item state syntax_node
  else
    lower_module_binding
      state
      ~syntax_node
      ~module_name:(Cst.ModuleStructure.name declaration)
      ~module_type:(Cst.ModuleStructure.module_type declaration)
      ~module_expression:(Cst.ModuleStructure.module_expression declaration)

and lower_module_signature_declaration = fun (state: state) (declaration: Cst.ModuleSignature.t) ->
  let syntax_node = Cst.ModuleSignature.syntax_node declaration in
  if
    Cst.ModuleSignature.is_recursive declaration
    || Option.is_some (Cst.ModuleSignature.next_and_declaration declaration)
    || not (List.is_empty (Cst.ModuleSignature.functor_parameters declaration))
  then
    add_unsupported_signature_item state syntax_node
  else
    let module_name = Cst.ModuleSignature.name declaration in
    match Cst.ModuleSignature.definition declaration with
    | Cst.ModuleSignature.Alias module_expression -> (
        match module_path_segments_of_module_expression state module_expression with
        | Some path when IdentPath.is_empty path ->
            ()
        | Some module_path ->
            let _ = add_item state ~syntax_node (`ModuleAlias (module_name, module_path)) in
            ()
        | None ->
            add_unsupported_signature_item state syntax_node
      )
    | Cst.ModuleSignature.Signature module_type ->
        let nested_scope_path = IdentPath.append_name state.scope_path module_name in
        with_scope
          state
          nested_scope_path
          (fun () ->
            lower_signature_items_of_module_type
              state
              module_type
              ~on_unsupported:(fun () -> add_unsupported_signature_item state syntax_node))

let lower_source_file = fun ~source source_file ->
  let state = make_state source in
  let () =
    match source_file with
    | Cst.Implementation implementation ->
        let _items = implementation.items |> List.map (lower_structure_item state) in
        ()
    | Cst.Interface interface ->
        let _items = interface.items |> List.map (lower_signature_item state) in
        ()
  in
  {
    SemanticTree.item_tree = ItemTree.of_list (List.rev state.items);
    body_arena = BodyArena.of_lists
      ~patterns:(List.rev state.patterns)
      ~expressions:(List.rev state.expressions)
      ~bindings:(List.rev state.bindings);
    origin_map = OriginMap.of_list (List.rev state.origins);
    diagnostics = List.rev state.diagnostics
  }
