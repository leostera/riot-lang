open Std
open Std.Collections
open Std.Data
module Iterator = Iter.Iterator

module Env = struct
  module Names = struct
    type t = string list

    let empty = []

    let singleton = fun name -> [ name ]

    let union = fun left right ->
      List.unique (List.sort (left @ right) ~compare:String.compare) ~compare:String.compare

    let elements = fun names -> names
  end

  type node =
    Node of Names.t * t

  and t = (string * node) list

  let empty = []

  let bound = Node (Names.empty, [])

  let singleton_name = Names.singleton

  let make_leaf name = Node (Names.singleton name, [])

  let make_node map = Node (Names.empty, map)

  let rec remove = fun name ->
    function
    | [] -> []
    | (key, _) :: rest when key = name -> rest
    | entry :: rest -> entry :: remove name rest

  let add = fun name node env -> (name, node) :: remove name env

  let merge = fun left right ->
    List.fold_left right ~init:left ~fn:(fun env (name, node) -> add name node env)

  let rec rebind = fun free_names ->
    function
    | Node (_, children) -> Node (
      free_names,
      List.map children ~fn:(fun (name, child) -> (name, rebind free_names child))
    )

  let rebind_exports = fun free_names exports ->
    List.map exports ~fn:(fun (name, node) -> (name, rebind free_names node))

  let rec add_path = fun env ~path ~free_names ->
    match path with
    | [] -> env
    | segment :: rest ->
        let existing =
          match
            List.find env
              ~fn:(fun (name, _) ->
                String.equal name segment)
          with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children =
          match rest with
          | [] -> children
          | _ -> add_path children ~path:rest ~free_names
        in
        add segment (Node (Names.union free free_names, updated_children)) env

  let rec add_binding = fun env ~path ~free_names ~exports ->
    match path with
    | [] ->
        env
    | [ segment ] ->
        let existing =
          match
            List.find env
              ~fn:(fun (name, _) ->
                String.equal name segment)
          with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let merged_children = merge children (rebind_exports free_names exports) in
        add segment (Node (Names.union free free_names, merged_children)) env
    | segment :: rest ->
        let existing =
          match
            List.find env
              ~fn:(fun (name, _) ->
                String.equal name segment)
          with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children = add_binding children ~path:rest ~free_names ~exports in
        add segment (Node (free, updated_children)) env

  let rec add_scoped_binding = fun env ~path ~free_names ~exports ->
    match path with
    | [] ->
        env
    | [ segment ] ->
        let existing =
          match
            List.find env
              ~fn:(fun (name, _) ->
                String.equal name segment)
          with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let merged_children = merge children exports in
        add segment (Node (Names.union free free_names, merged_children)) env
    | segment :: rest ->
        let existing =
          match
            List.find env
              ~fn:(fun (name, _) ->
                String.equal name segment)
          with
          | Some (_, node) -> node
          | None -> Node (Names.empty, [])
        in
        let Node (free, children) = existing in
        let updated_children = add_scoped_binding children ~path:rest ~free_names ~exports in
        add segment (Node (free, updated_children)) env

  let top_free = function
    | Node (free, _) -> free

  let children = function
    | Node (_, children) -> children

  let rec collect_free = function
    | Node (free, children) ->
        List.fold_left children ~init:free
          ~fn:(fun acc (_, child) ->
            Names.union acc (collect_free child))

  let merge_children env node = merge env (children node)

  let find = fun name env ->
    List.find env
      ~fn:(fun (key, _) ->
        String.equal key name) |> Option.map ~fn:(fun (_, value) -> value)

  let rec lookup_free segments env =
    match segments with
    | [] -> None
    | segment :: rest ->
        match find segment env with
        | None -> None
        | Some (Node (free, children)) -> (
            match rest with
            | [] -> Some free
            | _ -> (
                match lookup_free rest children with
                | Some free -> Some free
                | None -> Some free
              )
          )

  let rec lookup_map segments env =
    match segments with
    | [] ->
        None
    | [ segment ] ->
        find segment env
    | segment :: rest ->
        match find segment env with
        | None -> None
        | Some (Node (_, children)) -> lookup_map rest children

  let open_path = fun env ~path ->
    match lookup_map path env with
    | Some node -> merge_children env node
    | None -> env
end

module DepSet = struct
  type t = string HashSet.t

  let empty = fun () -> HashSet.create ()

  let add = fun deps name ->
    let _ = HashSet.insert deps ~value:name in
    deps

  let add_names = fun deps names ->
    List.for_each names
      ~fn:(fun name ->
        let _ = HashSet.insert deps ~value:name in
        ());
    deps

  let elements = fun deps -> HashSet.to_list deps |> List.sort ~compare:String.compare
end

type t = {
  modules: string list;
  env: Env.t;
  exports: Env.t;
}

type parse_error =
  | Parse_diagnostics of Diagnostic.t list
  | Cst_builder_error of Cst_builder.error

let ( let* ) = fun result f ->
  match result with
  | Ok value -> f value
  | Error _ as error -> error

let modules = fun t -> t.modules

let env = fun t -> t.env

let exports = fun t -> t.exports

let to_json = fun t ->
  Json.Object [ ("modules", Json.Array (List.map t.modules ~fn:(fun name -> Json.String name))) ]

let segments_of_ident = fun ident -> Cst.Ident.segments ident |> List.map ~fn:Cst.Token.text

let drop_last = function
  | [] ->
      []
  | [ _ ] ->
      []
  | items ->
      let rec loop acc = function
        | []
        | [ _ ] -> List.reverse acc
        | head :: tail -> loop (head :: acc) tail
      in
      loop [] items

let is_uppercase_ascii = fun ch -> ch >= 'A' && ch <= 'Z'

let is_module_head = fun segment ->
  String.length segment > 0 && is_uppercase_ascii (String.get_unchecked segment ~at:0)

let add_names = DepSet.add_names

let add_path = fun env deps segments ->
  match segments with
  | [] ->
      deps
  | head :: _ when is_module_head head ->
      let names =
        match Env.lookup_free segments env with
        | Some names -> names
        | None -> Env.singleton_name head
      in
      add_names deps names
  | _ ->
      deps

let add_parent = fun env deps ident ->
  let segments = segments_of_ident ident |> drop_last in
  add_path env deps segments

let add_module_path = fun env deps ident -> add_path env deps (segments_of_ident ident)

let rec module_like_segments_of_expression = function
  | Cst.Expression.Path { path; _ } -> (
      match segments_of_ident path with
      | head :: _ as segments when is_module_head head -> Some segments
      | _ -> None
    )
  | Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match module_like_segments_of_expression receiver with
      | Some segments -> Some (segments @ [ Cst.Token.text field_name ])
      | None -> None
    )
  | _ ->
      None

let collect_option = fun f env deps value ->
  match value with
  | Some value -> f env deps value
  | None -> Ok deps

let collect_list = fun f env deps values ->
  List.fold_left values ~init:(Ok deps)
    ~fn:(fun acc value ->
      let* deps = acc in
      f env deps value)

let collect_list_with = fun f acc values ->
  List.fold_left values ~init:(Ok acc)
    ~fn:(fun acc value ->
      let* acc = acc in
      f acc value)

let module_alias = fun env deps ident ->
  let deps = add_module_path env deps ident in
  let binding =
    match Env.lookup_map (segments_of_ident ident) env with
    | Some node -> node
    | None -> (
        match segments_of_ident ident with
        | [ name ] -> Env.make_leaf name
        | _ -> Env.bound
      )
  in
  (deps, binding)

let open_alias = fun env deps ident ->
  let deps, binding = module_alias env deps ident in
  let deps = add_names deps (Env.top_free binding) in
  (deps, Env.merge_children env binding)

let rec collect_core_type env deps type_ =
  match type_ with
  | Cst.CoreType.Wildcard _ ->
      Ok deps
  | Cst.CoreType.Var _ ->
      Ok deps
  | Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_list collect_core_type env deps arguments
  | Cst.CoreType.Class { class_path; arguments; _ } ->
      let deps = add_parent env deps class_path in
      collect_list collect_core_type env deps arguments
  | Cst.CoreType.Alias { type_; _ } ->
      collect_core_type env deps type_
  | Cst.CoreType.Attribute { type_; _ } ->
      collect_core_type env deps type_
  | Cst.CoreType.Extension _ ->
      Ok deps
  | Cst.CoreType.Poly { body; _ } ->
      collect_core_type env deps body
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      let* deps = collect_core_type env deps parameter_type in
      collect_core_type env deps result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      collect_list collect_core_type env deps elements
  | Cst.CoreType.Parenthesized { inner; _ } ->
      collect_core_type env deps inner
  | Cst.CoreType.PolyVariant poly_variant ->
      collect_list collect_row_field env deps poly_variant.fields
  | Cst.CoreType.Record { fields; _ } ->
      collect_list collect_inline_record_type_field env deps fields
  | Cst.CoreType.FirstClassModule { package_type; _ } ->
      collect_package_type env deps package_type
  | Cst.CoreType.Object { fields; _ } ->
      collect_list collect_object_type_field env deps fields

and collect_row_field env deps field =
  match field with
  | Cst.RowField.Tag tag -> collect_option collect_core_type env deps tag.payload_type
  | Cst.RowField.Inherit { type_; _ } -> collect_core_type env deps type_

and collect_module_type_constraint env deps constraint_ =
  let* deps = collect_core_type env deps constraint_.Cst.ModuleTypeConstraint.constrained_type in
  collect_core_type env deps constraint_.replacement_type

and collect_package_type env deps (package_type: Cst.package_type) =
  let deps = add_parent env deps package_type.module_type_path in
  collect_list collect_module_type_constraint env deps package_type.constraints

and collect_type_constraint env deps constraint_ =
  let* deps = collect_core_type env deps constraint_.Cst.TypeConstraint.left in
  collect_core_type env deps constraint_.right

and collect_inline_record_type_field env deps (field: Cst.record_type_field) = collect_core_type
  env
  deps
  field.field_type

and collect_record_type_field env deps (field: Cst.RecordField.t) = collect_core_type
  env
  deps
  field.field_type

and collect_object_type_field env deps (field: Cst.object_type_field) = collect_core_type
  env
  deps
  field.field_type

and collect_variant_constructor_arguments env deps arguments =
  match arguments with
  | Cst.ConstructorArguments.Tuple types -> collect_list collect_core_type env deps types
  | Cst.ConstructorArguments.Record { fields; _ } -> collect_list
    collect_record_type_field
    env
    deps
    fields

and collect_variant_constructor env deps (constructor: Cst.VariantConstructor.t) =
  let* deps =
    match constructor.arguments with
    | Some arguments -> collect_variant_constructor_arguments env deps arguments
    | None -> Ok deps
  in
  let* deps = collect_option collect_core_type env deps constructor.payload_type in
  collect_option collect_core_type env deps constructor.result_type

and collect_type_extension env deps (extension: Cst.TypeExtension.t) =
  let deps = add_parent env deps extension.type_name in
  collect_list collect_variant_constructor env deps extension.constructors

and collect_type_definition env deps definition =
  match definition with
  | Cst.TypeDefinition.Abstract -> Ok deps
  | Cst.TypeDefinition.Alias { manifest; _ } -> collect_core_type env deps manifest
  | Cst.TypeDefinition.Extensible _ -> Ok deps
  | Cst.TypeDefinition.FirstClassModule { package_type; _ } -> collect_package_type env deps package_type
  | Cst.TypeDefinition.Object { fields; _ } -> collect_list collect_object_type_field env deps fields
  | Cst.TypeDefinition.Record { fields; _ } -> collect_list collect_record_type_field env deps fields
  | Cst.TypeDefinition.Variant { constructors; _ } -> collect_list
    collect_variant_constructor
    env
    deps
    constructors
  | Cst.TypeDefinition.PolyVariant poly_variant -> collect_list
    collect_row_field
    env
    deps
    poly_variant.fields

and collect_type_declaration env deps declaration =
  let* deps = collect_option collect_core_type env deps declaration.Cst.TypeDeclaration.manifest_alias in
  let* deps = collect_type_definition env deps declaration.type_definition in
  let* deps = collect_list collect_type_constraint env deps declaration.constraints in
  match Cst.TypeDeclaration.next_and_declaration declaration with
  | Some next -> collect_type_declaration env deps next
  | None -> Ok deps

and collect_functor_parameters env deps parameters =
  List.fold_left parameters ~init:(Ok (deps, env))
    ~fn:(fun acc parameter ->
      let* (deps, env) = acc in
      let* deps = collect_module_type env deps parameter.Cst.FunctorParameter.module_type in
      let env = Env.add (Cst.Token.text parameter.name_token) Env.bound env in
      Ok (deps, env))

and module_type_binding env deps module_type =
  match module_type with
  | Cst.ModuleType.Path path ->
      Ok (add_parent env deps path, Env.bound)
  | Cst.ModuleType.TypeOf { module_path; _ } ->
      let deps, binding = module_alias env deps module_path in
      Ok (deps, binding)
  | Cst.ModuleType.Signature _ ->
      let* items = Cst_builder.signature_items_of_module_type module_type in
      let* (deps, _, bindings) = collect_signature_binding env deps items in
      Ok (deps, Env.make_node bindings)
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_module_type env deps result in
      Ok (deps, Env.bound)
  | Cst.ModuleType.With { base; constraints; _ } ->
      let* deps = collect_module_type env deps base in
      let* deps = collect_list collect_module_type_constraint env deps constraints in
      Ok (deps, Env.bound)
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      module_type_binding env deps inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      module_type_binding env deps module_type
  | Cst.ModuleType.Extension _ ->
      Ok (deps, Env.bound)

and collect_module_type env deps module_type =
  match module_type with
  | Cst.ModuleType.Path path ->
      Ok (add_parent env deps path)
  | Cst.ModuleType.TypeOf { module_path; _ } ->
      Ok (add_module_path env deps module_path)
  | Cst.ModuleType.Signature _ ->
      let* items = Cst_builder.signature_items_of_module_type module_type in
      let* (deps, _, _) = collect_signature_binding env deps items in
      Ok deps
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      collect_module_type env deps result
  | Cst.ModuleType.With { base; constraints; _ } ->
      let* deps = collect_module_type env deps base in
      collect_list collect_module_type_constraint env deps constraints
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      collect_module_type env deps inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      collect_module_type env deps module_type
  | Cst.ModuleType.Extension _ ->
      Ok deps

and module_binding env deps module_expression =
  match module_expression with
  | Cst.ModuleExpression.Path path ->
      let deps, binding = module_alias env deps path in
      Ok (deps, binding)
  | Cst.ModuleExpression.Structure _ ->
      let* items = Cst_builder.structure_items_of_module_expression module_expression in
      let* (deps, _, bindings) = collect_structure_binding env deps items in
      Ok (deps, Env.make_node bindings)
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_module_expression env deps body in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let* deps = collect_module_expression env deps callee in
      let* deps = collect_module_expression env deps argument in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      let* deps = collect_module_expression env deps callee in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      let* deps = collect_module_expression env deps module_expression in
      let* deps = collect_module_type env deps module_type in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.ModuleUnpack { expression; _ } ->
      let* deps = collect_expression env deps expression in
      Ok (deps, Env.bound)
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      module_binding env deps inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      module_binding env deps module_expression
  | Cst.ModuleExpression.Extension _ ->
      Ok (deps, Env.bound)

and collect_module_expression env deps module_expression =
  match module_expression with
  | Cst.ModuleExpression.Path path ->
      Ok (add_module_path env deps path)
  | Cst.ModuleExpression.Structure _ ->
      let* items = Cst_builder.structure_items_of_module_expression module_expression in
      let* (deps, _) = collect_structure env deps items in
      Ok deps
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      collect_module_expression env deps body
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let* deps = collect_module_expression env deps callee in
      collect_module_expression env deps argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      collect_module_expression env deps callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      let* deps = collect_module_expression env deps module_expression in
      collect_module_type env deps module_type
  | Cst.ModuleExpression.ModuleUnpack { expression; _ } ->
      collect_expression env deps expression
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      collect_module_expression env deps inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      collect_module_expression env deps module_expression
  | Cst.ModuleExpression.Extension _ ->
      Ok deps

and collect_apply_argument env deps argument =
  match argument with
  | Cst.Positional expression -> collect_expression env deps expression
  | Cst.Labeled { value; _ } -> collect_option collect_expression env deps value
  | Cst.Optional { value; _ } -> collect_option collect_expression env deps value

and bind_pattern_modules env pattern =
  match pattern with
  | Cst.Pattern.Identifier _ ->
      env
  | Cst.Pattern.Wildcard _ ->
      env
  | Cst.Pattern.Extension _ ->
      env
  | Cst.Pattern.Literal _ ->
      env
  | Cst.Pattern.Range _ ->
      env
  | Cst.Pattern.Operator _ ->
      env
  | Cst.Pattern.Lazy { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.FirstClassModule { binding; _ } -> (
      match binding with
      | Cst.Named { name_token } -> Env.add (Cst.Token.text name_token) Env.bound env
      | Cst.Anonymous _ -> env
    )
  | Cst.Pattern.PolyVariant { payload; _ } -> (
      match payload with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Pattern.PolyVariantInherit _ ->
      env
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.fold_left arguments ~init:env ~fn:bind_pattern_modules
  | Cst.Pattern.Tuple { elements; _ } ->
      List.fold_left
        elements
        ~init:env
        ~fn:(fun env (element: Cst.tuple_pattern_element) -> bind_pattern_modules env element.pattern)
  | Cst.Pattern.List { elements; _ } ->
      List.fold_left elements ~init:env ~fn:bind_pattern_modules
  | Cst.Pattern.Array { elements; _ } ->
      List.fold_left elements ~init:env ~fn:bind_pattern_modules
  | Cst.Pattern.Record { fields; _ } ->
      List.fold_left fields ~init:env
        ~fn:(fun env (field: Cst.record_pattern_field) ->
          match field.pattern with
          | Some pattern -> bind_pattern_modules env pattern
          | None -> env)
  | Cst.Pattern.Cons { head; tail; _ } ->
      let env = bind_pattern_modules env head in
      bind_pattern_modules env tail
  | Cst.Pattern.Or { alternatives; _ } ->
      List.fold_left alternatives ~init:env ~fn:bind_pattern_modules
  | Cst.Pattern.Alias { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Typed { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      let env = bind_pattern_modules env effect_pattern in
      bind_pattern_modules env continuation
  | Cst.Pattern.LocalOpen { pattern; _ } ->
      bind_pattern_modules env pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      bind_pattern_modules env inner

and bind_parameter_modules env parameter =
  match parameter with
  | Cst.Parameter.Positional positional ->
      bind_pattern_modules env positional.pattern
  | Cst.Parameter.Labeled labeled -> (
      match labeled.binding_pattern with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Parameter.Optional optional -> (
      match optional.binding_pattern with
      | Some pattern -> bind_pattern_modules env pattern
      | None -> env
    )
  | Cst.Parameter.LocallyAbstract _ ->
      env

and collect_parameters env deps parameters =
  List.fold_left parameters ~init:(Ok (deps, env))
    ~fn:(fun acc parameter ->
      let* (deps, env) = acc in
      let* deps = collect_parameter env deps parameter in
      let env = bind_parameter_modules env parameter in
      Ok (deps, env))

and bind_let_binding_chain_modules env (binding: Cst.let_binding) =
  let env = bind_pattern_modules env binding.binding_pattern in
  match binding.and_binding with
  | Some binding -> bind_let_binding_chain_modules env binding
  | None -> env

and bind_binding_operator_modules env (binding: Cst.binding_operator_binding) =
  let env = bind_pattern_modules env binding.binding_pattern in
  match binding.and_binding with
  | Some binding -> bind_binding_operator_modules env binding
  | None -> env

and collect_pattern env deps pattern =
  match pattern with
  | Cst.Pattern.Identifier _ ->
      Ok deps
  | Cst.Pattern.Wildcard _ ->
      Ok deps
  | Cst.Pattern.Extension _ ->
      Ok deps
  | Cst.Pattern.Literal _ ->
      Ok deps
  | Cst.Pattern.Lazy { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Range _ ->
      Ok deps
  | Cst.Pattern.Operator _ ->
      Ok deps
  | Cst.Pattern.FirstClassModule { package_type; _ } ->
      collect_option collect_package_type env deps package_type
  | Cst.Pattern.PolyVariant { payload; _ } ->
      collect_option collect_pattern env deps payload
  | Cst.Pattern.PolyVariantInherit { type_path; _ } ->
      Ok (add_parent env deps type_path)
  | Cst.Pattern.Constructor { constructor_path; arguments; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_list collect_pattern env deps arguments
  | Cst.Pattern.Tuple { elements; _ } ->
      collect_list
        (fun env deps (element: Cst.tuple_pattern_element) -> collect_pattern env deps element.pattern)
        env
        deps
        elements
  | Cst.Pattern.List { elements; _ } ->
      collect_list collect_pattern env deps elements
  | Cst.Pattern.Array { elements; _ } ->
      collect_list collect_pattern env deps elements
  | Cst.Pattern.Record { fields; _ } ->
      let deps =
        List.fold_left
          fields
          ~init:deps
          ~fn:(fun deps (field: Cst.record_pattern_field) -> add_parent env deps field.field_path)
      in
      collect_list
        (fun env deps (field: Cst.record_pattern_field) ->
          collect_option collect_pattern env deps field.pattern)
        env
        deps
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      let* deps = collect_pattern env deps head in
      collect_pattern env deps tail
  | Cst.Pattern.Or { alternatives; _ } ->
      collect_list collect_pattern env deps alternatives
  | Cst.Pattern.Alias { pattern; _ } ->
      collect_pattern env deps pattern
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      let* deps = collect_pattern env deps pattern in
      collect_core_type env deps type_
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      let* deps = collect_pattern env deps effect_pattern in
      collect_pattern env deps continuation
  | Cst.Pattern.LocalOpen { module_path; pattern; _ } ->
      let deps, env = open_alias env deps module_path in
      collect_pattern env deps pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      collect_pattern env deps inner

and collect_let_binding_chain env deps (binding: Cst.let_binding) =
  let* deps = collect_pattern env deps binding.binding_pattern in
  let* (deps, value_env) = collect_parameters env deps binding.parameters in
  let* deps = collect_expression value_env deps binding.Cst.LetBinding.value in
  collect_option collect_let_binding_chain env deps binding.and_binding

and collect_let_expression env deps (expression: Cst.let_expression) =
  let* deps = collect_pattern env deps expression.binding_pattern in
  let* (deps, value_env) = collect_parameters env deps expression.parameters in
  let* deps = collect_expression value_env deps expression.bound_value in
  let* deps = collect_option collect_let_binding_chain env deps expression.and_binding in
  let env = bind_pattern_modules env expression.binding_pattern in
  let env =
    match expression.and_binding with
    | Some binding -> bind_let_binding_chain_modules env binding
    | None -> env
  in
  collect_expression env deps expression.body

and collect_binding_operator env deps (binding: Cst.binding_operator_binding) =
  let* deps = collect_pattern env deps binding.binding_pattern in
  let* deps = collect_expression env deps binding.bound_value in
  collect_option collect_binding_operator env deps binding.and_binding

and collect_match_case env deps (case: Cst.match_case) =
  let* deps = collect_pattern env deps case.pattern in
  let env = bind_pattern_modules env case.pattern in
  let* deps = collect_option collect_expression env deps case.guard in
  collect_expression env deps case.body

and collect_parameter env deps parameter =
  match parameter with
  | Cst.Parameter.Positional positional ->
      collect_pattern env deps positional.pattern
  | Cst.Parameter.Labeled labeled ->
      collect_option collect_pattern env deps labeled.binding_pattern
  | Cst.Parameter.Optional optional ->
      let* deps = collect_option collect_pattern env deps optional.binding_pattern in
      collect_option collect_expression env deps optional.default_value
  | Cst.Parameter.LocallyAbstract _ ->
      Ok deps

and collect_expression env deps expression =
  match expression with
  | Cst.Expression.Path { path; _ } ->
      Ok (add_parent env deps path)
  | Cst.Expression.Constructor { constructor_path; payload; _ } ->
      let deps = add_parent env deps constructor_path in
      collect_option collect_expression env deps payload
  | Cst.Expression.Operator _ ->
      Ok deps
  | Cst.Expression.Literal _ ->
      Ok deps
  | Cst.Expression.Unreachable _ ->
      Ok deps
  | Cst.Expression.Extension _ ->
      Ok deps
  | Cst.Expression.Object _ ->
      Ok deps
  | Cst.Expression.PolyVariant { payload; _ } ->
      collect_option collect_expression env deps payload
  | Cst.Expression.ModulePack { module_expression; _ } ->
      collect_module_expression env deps module_expression
  | Cst.Expression.LetModule { module_name_token; module_expression; body; _ } ->
      let* (deps, binding) = module_binding env deps module_expression in
      let env = Env.add (Cst.Token.text module_name_token) binding env in
      collect_expression env deps body
  | Cst.Expression.LetException { body; _ } ->
      collect_expression env deps body
  | Cst.Expression.Assert { asserted; _ } ->
      collect_expression env deps asserted
  | Cst.Expression.Lazy { body; _ } ->
      collect_expression env deps body
  | Cst.Expression.While { condition; body; _ } ->
      let* deps = collect_expression env deps condition in
      collect_expression env deps body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      let* deps = collect_expression env deps start_expr in
      let* deps = collect_expression env deps end_expr in
      collect_expression env deps body
  | Cst.Expression.Apply { callee; argument; _ } ->
      let* deps = collect_expression env deps callee in
      collect_apply_argument env deps argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      collect_expression env deps receiver
  | Cst.Expression.New { class_path; _ } ->
      Ok (add_parent env deps class_path)
  | Cst.Expression.Prefix { operand; _ } ->
      collect_expression env deps operand
  | Cst.Expression.FieldAccess ({ receiver; _ } as field_access) -> (
      match module_like_segments_of_expression (Cst.Expression.FieldAccess field_access) with
      | Some segments -> Ok (add_path env deps (drop_last segments))
      | None -> collect_expression env deps receiver
    )
  | Cst.Expression.Index { collection; index; _ } ->
      let* deps = collect_expression env deps collection in
      collect_expression env deps index
  | Cst.Expression.ObjectOverride _ ->
      Ok deps
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      collect_expression env deps value
  | Cst.Expression.FieldAssign { target; value; _ } ->
      let* deps = collect_expression env deps target.receiver in
      collect_expression env deps value
  | Cst.Expression.Assign { target; value; _ } ->
      let* deps = collect_expression env deps target in
      collect_expression env deps value
  | Cst.Expression.Infix { left; right; _ } ->
      let* deps = collect_expression env deps left in
      collect_expression env deps right
  | Cst.Expression.TypeAscription { expression; kind; _ } ->
      let* deps = collect_expression env deps expression in
      (
        match kind with
        | Cst.Type { type_; _ } ->
            collect_core_type env deps type_
        | Cst.Coerce { type_; _ } ->
            collect_core_type env deps type_
        | Cst.ConstraintCoerce { from_type; to_type; _ } ->
            let* deps = collect_core_type env deps from_type in
            collect_core_type env deps to_type
      )
  | Cst.Expression.Polymorphic { expression; type_; _ } ->
      let* deps = collect_expression env deps expression in
      collect_core_type env deps type_
  | Cst.Expression.Sequence { expressions; _ } ->
      collect_list collect_expression env deps expressions
  | Cst.Expression.Tuple { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.List { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.Array { elements; _ } ->
      collect_list collect_expression env deps elements
  | Cst.Expression.Record (Cst.Literal { fields; _ }) ->
      let deps =
        List.fold_left
          fields
          ~init:deps
          ~fn:(fun deps (field: Cst.record_expression_field) -> add_parent env deps field.field_path)
      in
      collect_list
        (fun env deps (field: Cst.record_expression_field) -> collect_expression env deps field.value)
        env
        deps
        fields
  | Cst.Expression.Record (Cst.Update { base; fields; _ }) ->
      let* deps = collect_expression env deps base in
      let deps =
        List.fold_left
          fields
          ~init:deps
          ~fn:(fun deps (field: Cst.record_expression_field) -> add_parent env deps field.field_path)
      in
      collect_list
        (fun env deps (field: Cst.record_expression_field) -> collect_expression env deps field.value)
        env
        deps
        fields
  | Cst.Expression.LocalOpen (Cst.LetOpen { module_path; body; _ }) ->
      let deps, env = open_alias env deps module_path in
      collect_expression env deps body
  | Cst.Expression.LocalOpen (Cst.Delimited { module_path; body; _ }) ->
      let deps, env = open_alias env deps module_path in
      collect_expression env deps body
  | Cst.Expression.Fun { parameters; return_type; body; _ } ->
      let* (deps, env) = collect_parameters env deps parameters in
      let* deps = collect_option collect_core_type env deps return_type in
      (
        match body with
        | Cst.Expression expression -> collect_expression env deps expression
        | Cst.Cases cases -> collect_list collect_match_case env deps cases.cases
      )
  | Cst.Expression.Function { cases; _ } ->
      collect_list collect_match_case env deps cases
  | Cst.Expression.LetOperator { binding; body; _ } ->
      let* deps = collect_binding_operator env deps binding in
      let env = bind_binding_operator_modules env binding in
      collect_expression env deps body
  | Cst.Expression.Let let_expression ->
      collect_let_expression env deps let_expression
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      let* deps = collect_expression env deps scrutinee in
      collect_list collect_match_case env deps cases
  | Cst.Expression.Try { body; cases; _ } ->
      let* deps = collect_expression env deps body in
      collect_list collect_match_case env deps cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      let* deps = collect_expression env deps condition in
      let* deps = collect_expression env deps then_branch in
      collect_option collect_expression env deps else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      collect_expression env deps inner

and collect_structure env deps items =
  let* (deps, env, bindings) = collect_structure_binding env deps items in
  let deps = add_names deps (Env.collect_free (Env.make_node bindings)) in
  Ok (deps, env)

and collect_structure_binding env deps items =
  List.fold_left items ~init:(Ok (deps, env, Env.empty))
    ~fn:(fun acc item ->
      let* (deps, env, bindings) = acc in
      collect_structure_item env deps bindings item)

and module_structure_binding env deps (declaration: Cst.ModuleStructure.t) =
  match declaration.functor_parameters with
  | [] ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      module_binding env deps declaration.module_expression
  | parameters ->
      let* (deps, env) = collect_functor_parameters env deps parameters in
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      let* deps = collect_module_expression env deps declaration.module_expression in
      Ok (deps, Env.bound)

and collect_module_structure_rhs env deps (declaration: Cst.ModuleStructure.t) =
  let* (deps, env) = collect_functor_parameters env deps declaration.functor_parameters in
  let* deps = collect_option collect_module_type env deps declaration.module_type in
  collect_module_expression env deps declaration.module_expression

and prebind_module_structure_group env bindings (declaration: Cst.ModuleStructure.t) =
  let name = Cst.ModuleStructure.name declaration in
  let env = Env.add name Env.bound env in
  let bindings = Env.add name Env.bound bindings in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> prebind_module_structure_group env bindings next
  | None -> (env, bindings)

and collect_recursive_module_structure_group env deps (declaration: Cst.ModuleStructure.t) =
  let* deps = collect_module_structure_rhs env deps declaration in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> collect_recursive_module_structure_group env deps next
  | None -> Ok deps

and collect_sequential_module_structure_group env deps bindings (declaration: Cst.ModuleStructure.t) =
  let* (deps, binding) = module_structure_binding env deps declaration in
  let name = Cst.ModuleStructure.name declaration in
  let env = Env.add name binding env in
  let bindings = Env.add name binding bindings in
  match Cst.ModuleStructure.next_and_declaration declaration with
  | Some next -> collect_sequential_module_structure_group env deps bindings next
  | None -> Ok (deps, env, bindings)

and collect_structure_module_group env deps bindings (declaration: Cst.ModuleStructure.t) =
  if Cst.ModuleStructure.is_recursive declaration then
    let env, bindings = prebind_module_structure_group env bindings declaration in
    let* deps = collect_recursive_module_structure_group env deps declaration in
    Ok (deps, env, bindings)
  else
    collect_sequential_module_structure_group env deps bindings declaration

and collect_open_statement env deps (statement: Cst.OpenStatement.t) =
  match statement.Cst.OpenStatement.target with
  | Cst.OpenStatement.Path path -> Ok (open_alias env deps path)
  | Cst.OpenStatement.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.top_free binding) in
      Ok (deps, Env.merge_children env binding)

and collect_structure_include env deps (include_statement: Cst.include_statement) =
  match include_statement.Cst.target with
  | Cst.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.collect_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)
  | Cst.ModuleType module_type ->
      let* (deps, binding) = module_type_binding env deps module_type in
      let deps = add_names deps (Env.collect_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)

and collect_structure_item env deps bindings item =
  match item with
  | Cst.StructureItem.TypeDeclaration declaration ->
      let* deps = collect_type_declaration env deps declaration in
      Ok (deps, env, bindings)
  | Cst.StructureItem.TypeExtension extension ->
      let* deps = collect_type_extension env deps extension in
      Ok (deps, env, bindings)
  | Cst.StructureItem.LetBinding binding ->
      let* deps = collect_let_binding_chain env deps binding in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Expression expression ->
      let* deps = collect_expression env deps expression in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Attribute _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.Extension _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ClassDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ClassTypeDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ModuleDeclaration declaration ->
      collect_structure_module_group env deps bindings declaration
  | Cst.StructureItem.ModuleTypeDeclaration declaration ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      Ok (deps, env, bindings)
  | Cst.StructureItem.OpenStatement statement ->
      let* (deps, env) = collect_open_statement env deps statement in
      Ok (deps, env, bindings)
  | Cst.StructureItem.Docstring _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.Comment _ ->
      Ok (deps, env, bindings)
  | Cst.StructureItem.ExternalDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.StructureItem.IncludeStatement include_statement ->
      let* (deps, env) = collect_structure_include env deps include_statement in
      Ok (deps, env, bindings)
  | Cst.StructureItem.ExceptionDeclaration _ ->
      Ok (deps, env, bindings)

and collect_signature_binding env deps items =
  List.fold_left items ~init:(Ok (deps, env, Env.empty))
    ~fn:(fun acc item ->
      let* (deps, env, bindings) = acc in
      collect_signature_item env deps bindings item)

and module_signature_binding env deps (declaration: Cst.ModuleSignature.t) =
  let* (deps, env) = collect_functor_parameters env deps declaration.Cst.ModuleSignature.functor_parameters in
  match declaration.definition with
  | Cst.ModuleSignature.Signature module_type ->
      module_type_binding env deps module_type
  | Cst.ModuleSignature.Alias (Cst.ModuleExpression.Path path) ->
      let deps, binding = module_alias env deps path in
      Ok (deps, binding)
  | Cst.ModuleSignature.Alias module_expression ->
      let* deps = collect_module_expression env deps module_expression in
      Ok (deps, Env.bound)

and prebind_module_signature_group env bindings (declaration: Cst.ModuleSignature.t) =
  let name = Cst.ModuleSignature.name declaration in
  let env = Env.add name Env.bound env in
  let bindings = Env.add name Env.bound bindings in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> prebind_module_signature_group env bindings next
  | None -> (env, bindings)

and collect_recursive_module_signature_group env deps (declaration: Cst.ModuleSignature.t) =
  let* (deps, env_with_params) = collect_functor_parameters env deps declaration.functor_parameters in
  let* deps =
    match declaration.definition with
    | Cst.ModuleSignature.Signature module_type -> collect_module_type env_with_params deps module_type
    | Cst.ModuleSignature.Alias module_expression -> collect_module_expression env_with_params deps module_expression
  in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> collect_recursive_module_signature_group env deps next
  | None -> Ok deps

and collect_sequential_module_signature_group env deps bindings (declaration: Cst.ModuleSignature.t) =
  let* (deps, binding) = module_signature_binding env deps declaration in
  let name = Cst.ModuleSignature.name declaration in
  let env = Env.add name binding env in
  let bindings = Env.add name binding bindings in
  match Cst.ModuleSignature.next_and_declaration declaration with
  | Some next -> collect_sequential_module_signature_group env deps bindings next
  | None -> Ok (deps, env, bindings)

and collect_module_signature_group env deps bindings (declaration: Cst.ModuleSignature.t) =
  if Cst.ModuleSignature.is_recursive declaration then
    let env, bindings = prebind_module_signature_group env bindings declaration in
    let* deps = collect_recursive_module_signature_group env deps declaration in
    Ok (deps, env, bindings)
  else
    collect_sequential_module_signature_group env deps bindings declaration

and collect_signature_include env deps (include_statement: Cst.include_statement) =
  match include_statement.Cst.target with
  | Cst.ModuleType module_type ->
      let* (deps, binding) = module_type_binding env deps module_type in
      let deps = add_names deps (Env.top_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)
  | Cst.ModuleExpression module_expression ->
      let* (deps, binding) = module_binding env deps module_expression in
      let deps = add_names deps (Env.top_free binding) in
      let env = Env.merge_children env binding in
      Ok (deps, env)

and collect_signature_item env deps bindings item =
  match item with
  | Cst.SignatureItem.TypeDeclaration declaration ->
      let* deps = collect_type_declaration env deps declaration in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.TypeExtension extension ->
      let* deps = collect_type_extension env deps extension in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Attribute _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Extension _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ClassDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ClassTypeDeclaration _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ModuleDeclaration declaration ->
      collect_module_signature_group env deps bindings declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration ->
      let* deps = collect_option collect_module_type env deps declaration.module_type in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.OpenStatement statement ->
      let* (deps, env) = collect_open_statement env deps statement in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Docstring _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.Comment _ ->
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ValueDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ExternalDeclaration declaration ->
      let* deps = collect_core_type env deps declaration.type_ in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.IncludeStatement include_statement ->
      let* (deps, env) = collect_signature_include env deps include_statement in
      Ok (deps, env, bindings)
  | Cst.SignatureItem.ExceptionDeclaration _ ->
      Ok (deps, env, bindings)

module Ast2_deps = struct
  module A = Ast2

  let node_kind = A.Node.kind

  let token_kind = A.Token.kind

  let child_node_at = fun (node: A.Node.t) index ->
    match A.Node.child_at node index with
    | Some (Syntax_tree.Node id) -> Some ({ tree = node.tree; id }: A.Node.t)
    | Some (Syntax_tree.Token _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_at = fun (node: A.Node.t) index ->
    match A.Node.child_at node index with
    | Some (Syntax_tree.Token id) -> Some ({ tree = node.tree; id }: A.Token.t)
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_kind_is = fun node index kind ->
    match child_token_at node index with
    | Some token -> Syntax_kind2.(token_kind token = kind)
    | None -> false

  let node_kind_is = fun node kind -> Syntax_kind2.(node_kind node = kind)

  let is_module_expr_kind = function
    | Syntax_kind2.MODULE_EXPR
    | Syntax_kind2.PATH_MODULE_EXPR
    | Syntax_kind2.STRUCT_MODULE_EXPR
    | Syntax_kind2.FUNCTOR_MODULE_EXPR
    | Syntax_kind2.APPLY_MODULE_EXPR
    | Syntax_kind2.CONSTRAINT_MODULE_EXPR
    | Syntax_kind2.PAREN_MODULE_EXPR
    | Syntax_kind2.OPAQUE_MODULE_EXPR -> true
    | _ -> false

  let is_module_type_kind = function
    | Syntax_kind2.MODULE_TYPE_EXPR
    | Syntax_kind2.PATH_MODULE_TYPE
    | Syntax_kind2.SIGNATURE_MODULE_TYPE
    | Syntax_kind2.TYPEOF_MODULE_TYPE
    | Syntax_kind2.FUNCTOR_MODULE_TYPE
    | Syntax_kind2.WITH_MODULE_TYPE
    | Syntax_kind2.PAREN_MODULE_TYPE
    | Syntax_kind2.OPAQUE_MODULE_TYPE -> true
    | _ -> false

  let path_segments = fun node ->
    let segments = ref [] in
    A.Node.for_each_token node
      ~fn:(fun token ->
        if Syntax_kind2.(token_kind token = IDENT) then
          segments := A.Token.text token :: !segments);
    List.reverse !segments

  let add_parent_segments = fun env deps segments ->
    match drop_last segments with
    | head :: _ as parent when is_module_head head -> add_path env deps parent
    | _ -> deps

  let add_module_segments = fun env deps segments ->
    match segments with
    | head :: _ when is_module_head head -> add_path env deps segments
    | _ -> deps

  let module_alias = fun env deps segments ->
    let deps = add_module_segments env deps segments in
    let binding =
      match Env.lookup_map segments env with
      | Some node -> node
      | None -> (
          match segments with
          | [ name ] -> Env.make_leaf name
          | _ -> Env.bound
        )
    in
    (deps, binding)

  let open_alias = fun env deps segments ->
    let deps, binding = module_alias env deps segments in
    let deps = add_names deps (Env.top_free binding) in
    (deps, Env.merge_children env binding)

  let direct_path_between = fun node start stop ->
    let rec loop index expect_ident acc =
      if index >= stop then
        if not (List.is_empty acc) && not expect_ident then
          Some (List.reverse acc)
        else
          None
      else
        match child_token_at node index with
        | Some token when expect_ident && Syntax_kind2.(token_kind token = IDENT) -> loop
          (index + 1)
          false
          (A.Token.text token :: acc)
        | Some token when (not expect_ident) && Syntax_kind2.(token_kind token = DOT) -> loop
          (index + 1)
          true
          acc
        | _ -> None
    in
    loop start true []

  let collect_direct_module_refs_between = fun env deps node start stop ->
    let rec collect_path index acc =
      if index >= stop then
        (index, List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> collect_path
          (index + 1)
          (A.Token.text token :: acc)
        | Some token when Syntax_kind2.(token_kind token = DOT) -> collect_path (index + 1) acc
        | _ -> (index, List.reverse acc)
    in
    let rec scan index deps =
      if index >= stop then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) ->
            let next, segments = collect_path index [] in
            scan next (add_module_segments env deps segments)
        | _ -> scan (index + 1) deps
    in
    scan start deps

  let collect_direct_module_accesses_between = fun env deps node start stop ->
    let rec collect_path index acc saw_dot =
      if index >= stop then
        (index, List.reverse acc, saw_dot)
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> collect_path
          (index + 1)
          (A.Token.text token :: acc)
          saw_dot
        | Some token when Syntax_kind2.(token_kind token = DOT) -> collect_path (index + 1) acc true
        | _ -> (index, List.reverse acc, saw_dot)
    in
    let rec scan index deps =
      if index >= stop then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) ->
            let next, segments, saw_dot = collect_path index [] false in
            let deps =
              match segments with
              | head :: _ when saw_dot && is_module_head head -> add_parent_segments env deps segments
              | _ -> deps
            in
            scan next deps
        | _ -> scan (index + 1) deps
    in
    scan start deps

  let rec direct_module_binding_between env deps node start stop =
    match direct_path_between node start stop with
    | Some segments -> Ok (module_alias env deps segments)
    | None -> (
        match child_token_at node start with
        | Some token when Syntax_kind2.(token_kind token = STRUCT_KW) -> direct_struct_binding_between
          env
          deps
          node
          (start + 1)
          stop
        | _ ->
            let deps = collect_direct_module_refs_between env deps node start stop in
            Ok (deps, Env.bound)
      )

  and direct_struct_binding_between env deps node start stop =
    let deps = collect_direct_module_accesses_between env deps node start stop in
    let rec find_token index limit kind =
      if index >= limit then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        find_token (index + 1) limit kind
    in
    let rec member_stop index =
      if
        index >= stop
        || child_token_kind_is node index Syntax_kind2.MODULE_KW
        || child_token_kind_is node index Syntax_kind2.END_KW
      then
        index
      else
        member_stop (index + 1)
    in
    let rec scan index deps bindings =
      if index >= stop then
        Ok (deps, Env.make_node bindings)
      else if child_token_kind_is node index Syntax_kind2.MODULE_KW then
        let name =
          match child_token_at node (index + 1) with
          | Some token when Syntax_kind2.(token_kind token = IDENT) -> Some (A.Token.text token)
          | _ -> None
        in
        let member_stop =
          match find_token (index + 1) stop Syntax_kind2.EQ with
          | Some eq_index -> member_stop (eq_index + 1)
          | None -> member_stop (index + 1)
        in
        let* (deps, binding) =
          match find_token (index + 1) member_stop Syntax_kind2.EQ with
          | Some eq_index -> direct_module_binding_between env deps node (eq_index + 1) member_stop
          | None -> Ok (deps, Env.bound)
        in
        let bindings =
          match name with
          | Some name -> Env.add name binding bindings
          | None -> bindings
        in
        scan member_stop deps bindings
      else
        scan (index + 1) deps bindings
    in
    scan start deps Env.empty

  let first_child_node_matching = fun node ~matches ->
    let found = ref None in
    A.Node.for_each_child_node node
      ~fn:(fun child ->
        match !found with
        | Some _ -> ()
        | None ->
            if matches (node_kind child) then
              found := Some child);
    !found

  let rec unwrap_module_expr = fun node ->
    match node_kind node with
    | Syntax_kind2.MODULE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some child -> unwrap_module_expr child
        | None -> node
      )
    | _ -> node

  let rec unwrap_module_type = fun node ->
    match node_kind node with
    | Syntax_kind2.MODULE_TYPE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some child -> unwrap_module_type child
        | None -> node
      )
    | _ -> node

  let first_direct_child_node = fun node kind ->
    first_child_node_matching node ~matches:(fun child_kind -> Syntax_kind2.(child_kind = kind))

  let fold_child_nodes = fun node init fn ->
    let acc = ref init in
    A.Node.for_each_child_node node ~fn:(fun child -> acc := fn !acc child);
    !acc

  let collect_child_nodes = fun collect env deps node ->
    fold_child_nodes node (Ok deps)
      (fun acc child ->
        let* deps = acc in
        collect env deps child)

  let collect_direct_type_payload_paths = fun env deps node ->
    let count = A.Node.child_count node in
    let rec collect_path index acc =
      if index >= count then
        (index, List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> collect_path
          (index + 1)
          (A.Token.text token :: acc)
        | Some token when Syntax_kind2.(token_kind token = DOT) -> collect_path (index + 1) acc
        | _ -> (index, List.reverse acc)
    in
    let rec scan active index deps =
      if index >= count then
        deps
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = OF_KW || token_kind token = COLON) ->
            scan true (index + 1) deps
        | Some token when Syntax_kind2.(token_kind token = PIPE || token_kind token = AND_KW) ->
            scan false (index + 1) deps
        | Some token when active && Syntax_kind2.(token_kind token = IDENT) ->
            let next, segments = collect_path index [] in
            scan active next (add_parent_segments env deps segments)
        | _ ->
            scan active (index + 1) deps
    in
    scan false 0 deps

  let collect_direct_type_extension_path = fun env deps node ->
    let count = A.Node.child_count node in
    let rec find_plus index =
      if index >= count then
        None
      else if child_token_kind_is node index Syntax_kind2.PLUS then
        Some index
      else
        find_plus (index + 1)
    in
    let rec collect_before_plus index plus_index acc =
      if index >= plus_index then
        add_parent_segments env deps (List.reverse acc)
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> collect_before_plus
          (index + 1)
          plus_index
          (A.Token.text token :: acc)
        | Some token when Syntax_kind2.(token_kind token = DOT) -> collect_before_plus
          (index + 1)
          plus_index
          acc
        | _ -> collect_before_plus (index + 1) plus_index []
    in
    match find_plus 0 with
    | Some plus_index -> collect_before_plus 0 plus_index []
    | None -> deps

  let rec bind_pattern_modules env node =
    match node_kind node with
    | Syntax_kind2.FIRST_CLASS_MODULE_PATTERN ->
        let count = A.Node.child_count node in
        let rec find_binding index seen_module =
          if index >= count then
            None
          else
            match child_token_at node index with
            | Some token when Syntax_kind2.(token_kind token = MODULE_KW) -> find_binding
              (index + 1)
              true
            | Some token when seen_module && Syntax_kind2.(token_kind token = IDENT) -> Some (A.Token.text
              token)
            | Some token when seen_module && Syntax_kind2.(token_kind token = UNDERSCORE) -> None
            | _ -> find_binding (index + 1) seen_module
        in
        (
          match find_binding 0 false with
          | Some name -> Env.add name Env.bound env
          | None -> env
        )
    | _ -> fold_child_nodes node env (fun env child -> bind_pattern_modules env child)

  let rec collect_node env deps node =
    match node_kind node with
    | Syntax_kind2.PATH_EXPR
    | Syntax_kind2.PATH_PATTERN
    | Syntax_kind2.PATH_TYPE ->
        Ok (add_parent_segments env deps (path_segments node))
    | Syntax_kind2.PATH_MODULE_EXPR ->
        Ok (add_module_segments env deps (path_segments node))
    | Syntax_kind2.PATH_MODULE_TYPE ->
        Ok (add_parent_segments env deps (path_segments node))
    | Syntax_kind2.FIELD_ACCESS_EXPR ->
        let segments = path_segments node in
        let deps =
          match segments with
          | head :: _ when is_module_head head -> add_parent_segments env deps segments
          | _ -> deps
        in
        Ok deps
    | Syntax_kind2.LOCAL_OPEN_EXPR
    | Syntax_kind2.LOCAL_OPEN_PATTERN ->
        let segments = path_segments node in
        let deps, env =
          match segments with
          | head :: _ when is_module_head head -> open_alias env deps segments
          | _ -> (deps, env)
        in
        collect_child_nodes collect_node env deps node
    | Syntax_kind2.LET_MODULE_EXPR ->
        collect_let_module_expr env deps node
    | Syntax_kind2.FUN_EXPR ->
        collect_fun_expr env deps node
    | Syntax_kind2.TYPE_DECL ->
        let deps = collect_direct_type_extension_path env deps node in
        let deps = collect_direct_type_payload_paths env deps node in
        collect_child_nodes collect_node env deps node
    | Syntax_kind2.OPAQUE_TYPE ->
        Ok (collect_direct_module_refs_between env deps node 0 (A.Node.child_count node))
    | Syntax_kind2.STRUCTURE_ITEM ->
        let* (deps, _, _) = collect_structure_item env deps Env.empty node in
        Ok deps
    | Syntax_kind2.SIGNATURE_ITEM ->
        let* (deps, _, _) = collect_signature_item env deps Env.empty node in
        Ok deps
    | Syntax_kind2.MODULE_DECL ->
        let* (deps, _, _) = collect_module_decl env deps Env.empty node in
        Ok deps
    | Syntax_kind2.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps node in
        Ok deps
    | _ ->
        collect_child_nodes collect_node env deps node

  and collect_let_module_expr env deps node =
    let count = A.Node.child_count node in
    let rec find_name index =
      if index >= count then
        None
      else
        match child_token_at node index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> Some (A.Token.text token)
        | _ -> find_name (index + 1)
    in
    let rec find_node_after index ~matches =
      if index >= count then
        None
      else
        match child_node_at node index with
        | Some child when matches (node_kind child) -> Some child
        | _ -> find_node_after (index + 1) ~matches
    in
    let rec find_token index kind =
      if index >= count then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        find_token (index + 1) kind
    in
    let eq_index = find_token 0 Syntax_kind2.EQ in
    let in_index = find_token 0 Syntax_kind2.IN_KW in
    let body_expr =
      match in_index with
      | Some in_index -> find_node_after
        (in_index + 1)
        ~matches:(fun kind -> not (Syntax_kind2.(kind = MODULE_EXPR)))
      | None -> None
    in
    let module_expr =
      match eq_index, in_index with
      | Some eq_index, Some in_index -> find_node_between node (eq_index + 1) in_index ~matches:is_module_expr_kind
      | _ -> find_node_after 0 ~matches:is_module_expr_kind
    in
    let* (deps, binding) =
      match module_expr with
      | Some module_expr -> module_binding env deps module_expr
      | None -> (
          match eq_index, in_index with
          | Some eq_index, Some in_index -> direct_module_binding_between
            env
            deps
            node
            (eq_index + 1)
            in_index
          | _ -> Ok (deps, Env.bound)
        )
    in
    let env =
      match find_name 0 with
      | Some name -> Env.add name binding env
      | None -> env
    in
    match body_expr with
    | Some body -> collect_node env deps body
    | None -> Ok deps

  and collect_fun_expr env deps node =
    let count = A.Node.child_count node in
    let rec find_arrow index =
      if index >= count then
        None
      else if child_token_kind_is node index Syntax_kind2.ARROW then
        Some index
      else
        find_arrow (index + 1)
    in
    let rec collect_patterns index stop deps =
      if index >= stop then
        Ok deps
      else
        match child_node_at node index with
        | Some child ->
            let* deps = collect_node env deps child in
            collect_patterns (index + 1) stop deps
        | None -> collect_patterns (index + 1) stop deps
    in
    let rec bind_patterns index stop env =
      if index >= stop then
        env
      else
        match child_node_at node index with
        | Some child -> bind_patterns (index + 1) stop (bind_pattern_modules env child)
        | None -> bind_patterns (index + 1) stop env
    in
    match find_arrow 0 with
    | Some arrow_index ->
        let* deps = collect_patterns 0 arrow_index deps in
        let env = bind_patterns 0 arrow_index env in
        (
          match find_node_between node (arrow_index + 1) count ~matches:(fun _ -> true) with
          | Some body -> collect_node env deps body
          | None -> Ok deps
        )
    | None -> collect_child_nodes collect_node env deps node

  and collect_structure_items_in env deps node = collect_structure_binding env deps node

  and collect_signature_items_in env deps node = collect_signature_binding env deps node

  and module_binding env deps node =
    let node = unwrap_module_expr node in
    match node_kind node with
    | Syntax_kind2.PATH_MODULE_EXPR ->
        Ok (module_alias env deps (path_segments node))
    | Syntax_kind2.STRUCT_MODULE_EXPR ->
        let* (deps, _, bindings) = collect_structure_items_in env deps node in
        Ok (deps, Env.make_node bindings)
    | Syntax_kind2.CONSTRAINT_MODULE_EXPR
    | Syntax_kind2.PAREN_MODULE_EXPR -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some inner -> module_binding env deps inner
        | None ->
            let* deps = collect_node env deps node in
            Ok (deps, Env.bound)
      )
    | Syntax_kind2.FUNCTOR_MODULE_EXPR
    | Syntax_kind2.APPLY_MODULE_EXPR
    | Syntax_kind2.OPAQUE_MODULE_EXPR ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)
    | _ ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)

  and collect_module_expression env deps node =
    let* (deps, _) = module_binding env deps node in
    Ok deps

  and module_type_binding env deps node =
    let node = unwrap_module_type node in
    match node_kind node with
    | Syntax_kind2.PATH_MODULE_TYPE ->
        Ok (add_parent_segments env deps (path_segments node), Env.bound)
    | Syntax_kind2.SIGNATURE_MODULE_TYPE ->
        let* (deps, _, bindings) = collect_signature_items_in env deps node in
        Ok (deps, Env.make_node bindings)
    | Syntax_kind2.PAREN_MODULE_TYPE -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some inner -> module_type_binding env deps inner
        | None -> Ok (deps, Env.bound)
      )
    | Syntax_kind2.TYPEOF_MODULE_TYPE -> (
        match first_child_node_matching
          node
          ~matches:(fun kind -> is_module_expr_kind kind || Syntax_kind2.(kind = MODULE_EXPR)) with
        | Some module_expr -> module_binding env deps module_expr
        | None -> Ok (deps, Env.bound)
      )
    | Syntax_kind2.FUNCTOR_MODULE_TYPE
    | Syntax_kind2.WITH_MODULE_TYPE
    | Syntax_kind2.OPAQUE_MODULE_TYPE ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)
    | _ ->
        let* deps = collect_node env deps node in
        Ok (deps, Env.bound)

  and collect_module_type env deps node =
    let* (deps, _) = module_type_binding env deps node in
    Ok deps

  and collect_functor_head env deps member =
    let module Member = A.ModuleDeclaration.Member in
    let stop = Member.child_count member in
    let rec path_after_colon index acc =
      if index >= stop || Member.child_token_kind_is member index Syntax_kind2.RPAREN then
        add_parent_segments env deps (List.reverse acc)
      else
        match Member.child_token_at member index with
        | Some token when Syntax_kind2.(token_kind token = IDENT) -> path_after_colon
          (index + 1)
          (A.Token.text token :: acc)
        | _ -> path_after_colon (index + 1) acc
    in
    let rec scan index env deps =
      if index >= stop then
        (deps, env)
      else if Member.child_token_kind_is member index Syntax_kind2.LPAREN then
        let name =
          match Member.child_token_at member (index + 1) with
          | Some token when Syntax_kind2.(token_kind token = IDENT) -> Some (A.Token.text token)
          | _ -> None
        in
        let deps =
          let rec find_colon i =
            if i >= stop || Member.child_token_kind_is member i Syntax_kind2.RPAREN then
              deps
            else if Member.child_token_kind_is member i Syntax_kind2.COLON then
              path_after_colon (i + 1) []
            else
              find_colon (i + 1)
          in
          find_colon (index + 1)
        in
        let env =
          match name with
          | Some name -> Env.add name Env.bound env
          | None -> env
        in
        scan (index + 1) env deps
      else
        scan (index + 1) env deps
    in
    scan 0 env deps

  and module_member_name member =
    match A.ModuleDeclaration.Member.name member with
    | Some token -> Some (A.Token.text token)
    | None -> None

  and find_token_between node start stop kind =
    let rec loop index =
      if index >= stop then
        None
      else if child_token_kind_is node index kind then
        Some index
      else
        loop (index + 1)
    in
    loop start

  and find_node_between node start stop ~matches =
    let rec loop index =
      if index >= stop then
        None
      else
        match child_node_at node index with
        | Some child when matches (node_kind child) -> Some child
        | _ -> loop (index + 1)
    in
    loop start

  and prebind_module_decl_group env bindings node =
    A.ModuleDeclaration.fold_members node (env, bindings)
      (fun (env, bindings) member ->
        match module_member_name member with
        | Some name -> (Env.add name Env.bound env, Env.add name Env.bound bindings)
        | None -> (env, bindings))

  and collect_module_member_rhs env deps member =
    let deps, env = collect_functor_head env deps member in
    let* deps =
      match A.ModuleDeclaration.Member.module_type member with
      | Some module_type -> collect_module_type env deps module_type
      | None -> Ok deps
    in
    match A.ModuleDeclaration.Member.module_expr member with
    | Some module_expr -> collect_module_expression env deps module_expr
    | None -> Ok deps

  and module_member_binding env deps member =
    let deps, env = collect_functor_head env deps member in
    let* deps =
      match A.ModuleDeclaration.Member.module_type member with
      | Some module_type -> collect_module_type env deps module_type
      | None -> Ok deps
    in
    match A.ModuleDeclaration.Member.module_expr member with
    | Some module_expr -> module_binding env deps module_expr
    | None -> Ok (deps, Env.bound)

  and collect_module_decl env deps bindings node =
    if A.ModuleDeclaration.is_recursive node then
      let env, bindings = prebind_module_decl_group env bindings node in
      let* deps =
        A.ModuleDeclaration.fold_members node (Ok deps)
          (fun acc member ->
            let* deps = acc in
            collect_module_member_rhs env deps member)
      in
      Ok (deps, env, bindings)
    else
      A.ModuleDeclaration.fold_members node (Ok (deps, env, bindings))
        (fun acc member ->
          let* (deps, env, bindings) = acc in
          let* (deps, binding) = module_member_binding env deps member in
          match module_member_name member with
          | Some name ->
              let env = Env.add name binding env in
              let bindings = Env.add name binding bindings in
              Ok (deps, env, bindings)
          | None -> Ok (deps, env, bindings))

  and collect_module_type_decl env deps node =
    match first_child_node_matching
      node
      ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_TYPE_DECL_BODY)) with
    | Some body -> (
        match first_child_node_matching
          body
          ~matches:(fun kind -> is_module_type_kind kind || Syntax_kind2.(kind = MODULE_TYPE_EXPR)) with
        | Some module_type -> collect_module_type env deps module_type
        | None -> Ok deps
      )
    | None -> Ok deps

  and include_structure_binding env deps node =
    match first_child_node_matching node ~matches:is_module_expr_kind with
    | Some module_expr -> module_binding env deps module_expr
    | None -> (
        match first_child_node_matching node ~matches:is_module_type_kind with
        | Some module_type -> module_type_binding env deps module_type
        | None -> direct_module_binding_between env deps node 1 (A.Node.child_count node)
      )

  and include_signature_binding env deps node =
    match first_child_node_matching node ~matches:is_module_type_kind with
    | Some module_type -> module_type_binding env deps module_type
    | None -> (
        match first_child_node_matching node ~matches:is_module_expr_kind with
        | Some module_expr -> module_binding env deps module_expr
        | None -> (
            let count = A.Node.child_count node in
            match direct_path_between node 1 count with
            | Some segments -> Ok (add_parent_segments env deps segments, Env.bound)
            | None ->
                let deps = collect_direct_module_refs_between env deps node 1 count in
                Ok (deps, Env.bound)
          )
      )

  and collect_open_decl env deps node =
    let count = A.Node.child_count node in
    match direct_path_between node 1 count with
    | Some (head :: _ as segments) when is_module_head head ->
        Ok (open_alias env deps segments)
    | Some _ ->
        Ok (deps, env)
    | None ->
        let deps = collect_direct_module_refs_between env deps node 1 count in
        Ok (deps, env)

  and collect_include_structure env deps node =
    let* (deps, binding) = include_structure_binding env deps node in
    let deps = add_names deps (Env.collect_free binding) in
    Ok (deps, Env.merge_children env binding)

  and collect_include_signature env deps node =
    let* (deps, binding) = include_signature_binding env deps node in
    let deps = add_names deps (Env.top_free binding) in
    Ok (deps, Env.merge_children env binding)

  and collect_structure_item env deps bindings item =
    match first_child_node_matching item ~matches:(fun _ -> true) with
    | Some decl when node_kind_is decl Syntax_kind2.MODULE_DECL ->
        collect_module_decl env deps bindings decl
    | Some decl when node_kind_is decl Syntax_kind2.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind2.OPEN_DECL ->
        let* (deps, env) = collect_open_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind2.INCLUDE_DECL ->
        let* (deps, env) = collect_include_structure env deps decl in
        Ok (deps, env, bindings)
    | Some decl ->
        let* deps = collect_node env deps decl in
        Ok (deps, env, bindings)
    | None ->
        Ok (deps, env, bindings)

  and collect_signature_item env deps bindings item =
    match first_child_node_matching item ~matches:(fun _ -> true) with
    | Some decl when node_kind_is decl Syntax_kind2.MODULE_DECL ->
        collect_module_decl env deps bindings decl
    | Some decl when node_kind_is decl Syntax_kind2.MODULE_TYPE_DECL ->
        let* deps = collect_module_type_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind2.OPEN_DECL ->
        let* (deps, env) = collect_open_decl env deps decl in
        Ok (deps, env, bindings)
    | Some decl when node_kind_is decl Syntax_kind2.INCLUDE_DECL ->
        let* (deps, env) = collect_include_signature env deps decl in
        Ok (deps, env, bindings)
    | Some decl ->
        let* deps = collect_node env deps decl in
        Ok (deps, env, bindings)
    | None ->
        Ok (deps, env, bindings)

  and collect_structure_binding env deps node =
    fold_child_nodes node (Ok (deps, env, Env.empty))
      (fun acc child ->
        let* (deps, env, bindings) = acc in
        if node_kind_is child Syntax_kind2.STRUCTURE_ITEM then
          collect_structure_item env deps bindings child
        else
          Ok (deps, env, bindings))

  and collect_signature_binding env deps node =
    fold_child_nodes node (Ok (deps, env, Env.empty))
      (fun acc child ->
        let* (deps, env, bindings) = acc in
        if node_kind_is child Syntax_kind2.SIGNATURE_ITEM then
          collect_signature_item env deps bindings child
        else
          Ok (deps, env, bindings))

  let finalize_impl = fun env impl ->
    let* (deps, env, exports) = collect_structure_binding env (DepSet.empty ()) impl in
    let deps = add_names deps (Env.collect_free (Env.make_node exports)) in
    Ok (deps, env, exports)

  let finalize_intf = fun env intf ->
    let* (deps, env, exports) = collect_signature_binding env (DepSet.empty ()) intf in
    Ok (deps, env, exports)

  let of_parse2_result = fun ~env result ->
    match A.SourceFile.view (A.SourceFile.make result.Parser2.tree) with
    | A.SourceFile.Implementation impl -> finalize_impl env impl
    | A.SourceFile.Interface intf -> finalize_intf env intf
    | A.SourceFile.Empty -> Ok (DepSet.empty (), env, Env.empty)
end

let finalize = fun deps env exports -> { modules = DepSet.elements deps; env; exports }

let of_cst = fun ?(env = Env.empty) source_file ->
  match source_file with
  | Cst.Implementation implementation ->
      let* (deps, env, exports) = collect_structure_binding env (DepSet.empty ()) implementation.items in
      Ok (finalize deps env exports)
  | Cst.Interface interface ->
      let* (deps, env, exports) = collect_signature_binding env (DepSet.empty ()) interface.items in
      Ok (finalize deps env exports)

let of_parse2_result = fun ?(env = Env.empty) result ->
  if Int.(Vector.length result.Parser2.diagnostics != 0) then
    Error (Parse_diagnostics (Vector.iter result.Parser2.diagnostics |> Iterator.to_list))
  else
    match Ast2_deps.of_parse2_result ~env result with
    | Ok (deps, env, exports) -> Ok (finalize deps env exports)
    | Error err -> Error (Cst_builder_error err)

let of_parse_result = fun ?(env = Env.empty) result ->
  let source = IO.IoVec.IoSlice.from_string result.Parser.source |> Result.unwrap in
  let result =
    match result.Parser.kind with
    | `Implementation -> Parser2.parse_implementation source
    | `Interface -> Parser2.parse_interface source
  in
  of_parse2_result ~env result
