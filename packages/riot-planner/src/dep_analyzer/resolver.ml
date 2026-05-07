module Dep_env = Env

open Std
open Std.Collections
open Std.Result.Syntax

module Item = Ir.Item

type provider = {
  path: string list;
  free_names: string list;
  exports: string list list;
}

module Resolution = struct
  type t = {
    modules: string list;
    unresolved: string list;
  }

  let make = fun ~modules ~unresolved -> { modules; unresolved }

  let modules = fun t -> t.modules

  let unresolved = fun t -> t.unresolved
end

module ResolvedSource = struct
  type t = {
    source: Path.t;
    source_hash: Crypto.hash;
    module_path: string list option;
    modules: string list;
    unresolved: string list;
  }

  let source = fun t -> t.source

  let source_hash = fun t -> t.source_hash

  let module_path = fun t -> t.module_path

  let modules = fun t -> t.modules

  let unresolved = fun t -> t.unresolved
end

type error =
  | Invalid_provider of string

module DepSet = struct
  type t = string HashSet.t

  let empty = fun () -> HashSet.create ()

  let add = fun deps name ->
    let _ = HashSet.insert deps ~value:name in
    deps

  let add_names = fun deps names ->
    List.for_each
      names
      ~fn:(fun name ->
        let _ = HashSet.insert deps ~value:name in
        ());
    deps

  let elements = fun deps ->
    HashSet.to_list deps
    |> List.sort ~compare:String.compare
end

let add_names = DepSet.add_names

let add_path = fun env deps segments ->
  match segments with
  | [] -> deps
  | head :: _ when Ir.is_module_head head ->
      let names =
        match Dep_env.lookup_free ~use_open_fallback:true segments env with
        | Some names -> names
        | None -> Dep_env.singleton_name head
      in
      add_names deps names
  | _ -> deps

module Ast_deps = struct
  let add_module_segments = fun env deps segments ->
    match segments with
    | head :: _ when Ir.is_module_head head -> add_path env deps segments
    | _ -> deps

  let module_alias_for_segments = fun env deps segments ->
    let deps = add_module_segments env deps segments in
    let binding =
      match Dep_env.lookup_map segments env with
      | Some node -> (
          match Dep_env.top_free node with
          | [] -> node
          | free_names -> Dep_env.rebind free_names node
        )
      | None -> (
          match segments with
          | [ name ] -> Dep_env.make_leaf name
          | _ -> Dep_env.bound
        )
    in
    (deps, binding)

  let open_alias_for_segments = fun ?(fallback = false) env deps segments ->
    let binding_known = Option.is_some (Dep_env.lookup_map segments env) in
    let (deps, binding) = module_alias_for_segments env deps segments in
    let deps = add_names deps (Dep_env.top_free binding) in
    let env = Dep_env.merge_children env binding in
    let env =
      if fallback && binding_known && not (Dep_env.has_children binding) then
        match segments with
        | head :: _ when Ir.is_module_head head ->
            let free_names =
              match Dep_env.top_free binding with
              | [] -> [ head ]
              | names -> names
            in
            Dep_env.add_open_fallback env ~free_names
        | _ -> env
      else
        env
    in
    (deps, env)

  let rec collect_node env deps item =
    match item with
    | Item.Use segments -> Ok (add_module_segments env deps segments)
    | Item.Open expr ->
        let* (deps, _env) = collect_open_decl env deps expr in
        Ok deps
    | Item.Include (Item.Structure, expr) ->
        let* (deps, _env) = collect_include_structure env deps expr in
        Ok deps
    | Item.Include (Item.Signature, expr) ->
        let* (deps, _env) = collect_include_signature env deps expr in
        Ok deps
    | Item.Module _
    | Item.ModuleAlias _
    | Item.Functor _
    | Item.ModuleType _ ->
        let* (deps, _, _) = collect_structure_item env deps Dep_env.empty item in
        Ok deps
    | Item.FunctorApply { callee; argument } ->
        let* deps = collect_node env deps callee in
        collect_node env deps argument
    | Item.Constraint { expr; signature } ->
        let* deps = collect_signature_nodes env deps signature in
        collect_node env deps expr
    | Item.Typeof expr -> collect_node env deps expr
    | Item.WithConstraint { base; constraints } ->
        let* deps = collect_node env deps base in
        collect_structure_nodes env deps constraints
    | Item.BindModules { modules; scope } ->
        let* (deps, env) = collect_bound_modules env deps modules in
        let* (deps, _, _) = collect_structure_binding env deps scope in
        Ok deps
    | Item.Scope items ->
        let* (deps, _, _) = collect_structure_binding env deps items in
        Ok deps

  and module_binding env deps expr =
    match expr with
    | Item.Use segments -> Ok (module_alias_for_segments env deps segments)
    | Item.Scope items ->
        let* (deps, _, bindings) = collect_structure_items_in env deps items in
        Ok (deps, Dep_env.make_node bindings)
    | Item.Constraint { expr; signature } ->
        let* deps = collect_signature_nodes env deps signature in
        module_binding env deps expr
    | Item.FunctorApply { callee; argument } ->
        let* deps = collect_node env deps callee in
        let* deps = collect_node env deps argument in
        Ok (deps, Dep_env.bound)
    | Item.Typeof expr -> module_binding env deps expr
    | Item.WithConstraint { base; constraints } ->
        let* deps = collect_structure_nodes env deps constraints in
        module_binding env deps base
    | Item.Module { body; signature; _ } ->
        let* deps = collect_signature_nodes env deps signature in
        let* (deps, _, bindings) = collect_structure_items_in env deps body in
        Ok (deps, Dep_env.make_node bindings)
    | Item.ModuleAlias { target; _ } -> module_binding env deps target
    | Item.Functor { args; body; _ } ->
        let* (deps, env) = collect_functor_args env deps args in
        let* (deps, _, bindings) = collect_structure_items_in env deps body in
        Ok (deps, Dep_env.make_node bindings)
    | Item.ModuleType { body; _ } ->
        let* deps = collect_signature_nodes env deps body in
        Ok (deps, Dep_env.bound)
    | Item.Open _
    | Item.Include _
    | Item.BindModules _ ->
        let* deps = collect_node env deps expr in
        Ok (deps, Dep_env.bound)

  and module_type_binding env deps items =
    let* (deps, _, bindings) = collect_signature_items_in env deps items in
    Ok (deps, Dep_env.make_node bindings)

  and collect_module_expression env deps items =
    let* (deps, _) = module_binding env deps items in
    Ok deps

  and collect_module_type env deps items =
    let* (deps, _) = module_type_binding env deps items in
    Ok deps

  and collect_structure_items_in env deps items = collect_structure_binding env deps items

  and collect_signature_items_in env deps items = collect_signature_binding env deps items

  and collect_open_decl env deps expr =
    match expr with
    | Item.Use (head :: _ as segments) when Ir.is_module_head head ->
        Ok (open_alias_for_segments ~fallback:true env deps segments)
    | _ ->
        let* (deps, binding) = module_binding env deps expr in
        let deps = add_names deps (Dep_env.top_free binding) in
        Ok (deps, Dep_env.merge_children env binding)

  and collect_include_structure env deps expr =
    let* (deps, binding) = module_binding env deps expr in
    let deps = add_names deps (Dep_env.collect_free binding) in
    Ok (deps, Dep_env.merge_children env binding)

  and collect_include_signature env deps expr =
    let* (deps, binding) = module_binding env deps expr in
    let deps = add_names deps (Dep_env.top_free binding) in
    Ok (deps, Dep_env.merge_children env binding)

  and collect_structure_nodes env deps items =
    let* (deps, _, _) = collect_structure_binding env deps items in
    Ok deps

  and collect_signature_nodes env deps items =
    let* (deps, _, _) = collect_signature_binding env deps items in
    Ok deps

  and collect_functor_args env deps args =
    List.fold_left
      args
      ~init:(Ok (deps, env))
      ~fn:(fun acc (arg: Item.functor_arg) ->
        let* (deps, env) = acc in
        let* deps = collect_signature_nodes env deps arg.ascription in
        let env =
          match arg.name with
          | Some name -> Dep_env.add name Dep_env.bound env
          | None -> env
        in
        Ok (deps, env))

  and collect_bound_modules env deps modules =
    List.fold_left
      modules
      ~init:(Ok (deps, env))
      ~fn:(fun acc (module_: Item.bound_module) ->
        let* (deps, env) = acc in
        let* deps = collect_signature_nodes env deps module_.ascription in
        Ok (deps, Dep_env.add module_.name Dep_env.bound env))

  and collect_structure_item env deps bindings item =
    match item with
    | Item.Module { name; signature; body } ->
        let* deps = collect_signature_nodes env deps signature in
        let* (deps, _, module_bindings) = collect_structure_binding env deps body in
        let binding = Dep_env.make_node module_bindings in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.ModuleAlias { name; target } ->
        let* (deps, binding) = module_binding env deps target in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.Functor { name; args; body } ->
        let* (deps, functor_env) = collect_functor_args env deps args in
        let* (deps, _, module_bindings) = collect_structure_binding functor_env deps body in
        let binding = Dep_env.make_node module_bindings in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.ModuleType { body; _ } ->
        let* deps = collect_signature_nodes env deps body in
        Ok (deps, env, bindings)
    | Item.Open expr ->
        let* (deps, env) = collect_open_decl env deps expr in
        Ok (deps, env, bindings)
    | Item.Include (Item.Structure, expr) ->
        let* (deps, env) = collect_include_structure env deps expr in
        Ok (deps, env, bindings)
    | Item.Include (Item.Signature, expr) ->
        let* (deps, env) = collect_include_signature env deps expr in
        Ok (deps, env, bindings)
    | Item.BindModules { modules; scope } ->
        let* (deps, scope_env) = collect_bound_modules env deps modules in
        let* (deps, _, _) = collect_structure_binding scope_env deps scope in
        Ok (deps, env, bindings)
    | Item.Scope items ->
        let* (deps, _, _) = collect_structure_binding env deps items in
        Ok (deps, env, bindings)
    | Item.Use _
    | Item.FunctorApply _
    | Item.Constraint _
    | Item.Typeof _
    | Item.WithConstraint _ ->
        let* deps = collect_node env deps item in
        Ok (deps, env, bindings)

  and collect_signature_item env deps bindings item =
    match item with
    | Item.Module { name; signature; body } ->
        let* deps = collect_signature_nodes env deps signature in
        let* (deps, _, module_bindings) = collect_signature_binding env deps body in
        let binding = Dep_env.make_node module_bindings in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.ModuleAlias { name; target } ->
        let* (deps, binding) = module_binding env deps target in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.Functor { name; args; body } ->
        let* (deps, functor_env) = collect_functor_args env deps args in
        let* (deps, _, module_bindings) = collect_signature_binding functor_env deps body in
        let binding = Dep_env.make_node module_bindings in
        let env = Dep_env.add name binding env in
        let bindings = Dep_env.add name binding bindings in
        Ok (deps, env, bindings)
    | Item.ModuleType { body; _ } ->
        let* deps = collect_signature_nodes env deps body in
        Ok (deps, env, bindings)
    | Item.Open expr ->
        let* (deps, env) = collect_open_decl env deps expr in
        Ok (deps, env, bindings)
    | Item.Include (Item.Structure, expr) ->
        let* (deps, env) = collect_include_structure env deps expr in
        Ok (deps, env, bindings)
    | Item.Include (Item.Signature, expr) ->
        let* (deps, env) = collect_include_signature env deps expr in
        Ok (deps, env, bindings)
    | Item.BindModules { modules; scope } ->
        let* (deps, scope_env) = collect_bound_modules env deps modules in
        let* (deps, _, _) = collect_signature_binding scope_env deps scope in
        Ok (deps, env, bindings)
    | Item.Scope items ->
        let* (deps, _, _) = collect_signature_binding env deps items in
        Ok (deps, env, bindings)
    | Item.Use _
    | Item.FunctorApply _
    | Item.Constraint _
    | Item.Typeof _
    | Item.WithConstraint _ ->
        let* deps = collect_node env deps item in
        Ok (deps, env, bindings)

  and collect_structure_binding env deps items =
    List.fold_left
      items
      ~init:(Ok (deps, env, Dep_env.empty))
      ~fn:(fun acc item ->
        let* (deps, env, bindings) = acc in
        collect_structure_item env deps bindings item)

  and collect_signature_binding env deps items =
    List.fold_left
      items
      ~init:(Ok (deps, env, Dep_env.empty))
      ~fn:(fun acc item ->
        let* (deps, env, bindings) = acc in
        collect_signature_item env deps bindings item)

  let finalize_impl = fun env (summary: Ir.source_summary) ->
    let* (deps, env, exports) = collect_structure_binding env (DepSet.empty ()) summary.Ir.items in
    let deps = add_names deps (Dep_env.collect_free (Dep_env.make_node exports)) in
    Ok (deps, env, exports)

  let finalize_intf = fun env (summary: Ir.source_summary) ->
    let* (deps, env, exports) = collect_signature_binding env (DepSet.empty ()) summary.Ir.items in
    Ok (deps, env, exports)

  let from_parse_result = fun ~env (summary: Ir.source_summary) ->
    match summary.kind with
    | Ir.Implementation -> finalize_impl env summary
    | Ir.Interface -> finalize_intf env summary
end

let module_path_root_free_names = fun module_path ->
  match module_path with
  | root :: _ -> [ root ]
  | [] -> []

let summary_is_generated_alias = fun (summary: Ir.source_summary) ->
  match summary.module_path with
  | Some module_path -> (
      match List.reverse module_path with
      | name :: _ -> String.equal name "Aliases" || String.ends_with ~suffix:"__Aliases" name
      | _ -> false
    )
  | None -> false

let alias_summary_exports = fun (summary: Ir.source_summary) ->
  let rec replay_alias_items env exports items =
    List.fold_left
      items
      ~init:(env, exports)
      ~fn:(fun (env, exports) item ->
        match item with
        | Item.ModuleAlias { name; _ } ->
            let binding = Dep_env.make_leaf name in
            (Dep_env.add name binding env, Dep_env.add name binding exports)
        | Item.Module { name; body; _ } ->
            let (_, child_exports) = replay_alias_items env Dep_env.empty body in
            let binding = Dep_env.make_node child_exports in
            (Dep_env.add name binding env, Dep_env.add name binding exports)
        | Item.Functor { name; body; _ } ->
            let (_, child_exports) = replay_alias_items env Dep_env.empty body in
            let binding = Dep_env.make_node child_exports in
            (Dep_env.add name binding env, Dep_env.add name binding exports)
        | Item.ModuleType _
        | Item.FunctorApply _
        | Item.Constraint _
        | Item.Typeof _
        | Item.WithConstraint _
        | Item.BindModules _ -> (env, exports)
        | Item.Scope body ->
            let (_, _) = replay_alias_items env Dep_env.empty body in
            (env, exports)
        | Item.Use _
        | Item.Open _
        | Item.Include _ -> (env, exports))
  in
  let (_, exports) = replay_alias_items Dep_env.empty Dep_env.empty summary.items in
  exports

let summary_exports = fun env (summary: Ir.source_summary) ->
  if summary_is_generated_alias summary then
    alias_summary_exports summary
  else
    match Ast_deps.from_parse_result ~env summary with
    | Ok (_deps, _env, exports) -> exports
    | Error _ -> Dep_env.empty

let add_summary = fun ~full_free_names_of_module_path ~add_simple env summary ->
  match summary.Ir.module_path with
  | None
  | Some [] -> env
  | Some module_path ->
      let exports = summary_exports env summary in
      let add_binding =
        if summary_is_generated_alias summary then
          Dep_env.add_scoped_binding
        else
          Dep_env.add_binding
      in
      let env =
        add_binding
          env
          ~path:module_path
          ~free_names:(full_free_names_of_module_path module_path)
          ~exports
      in
      if add_simple then
        match List.reverse module_path with
        | simple :: _ -> add_binding env ~path:[ simple ] ~free_names:[ simple ] ~exports
        | [] -> env
      else
        env

let add_summaries = fun ~full_free_names_of_module_path ~add_simple env summaries ->
  List.fold_left
    summaries
    ~init:env
    ~fn:(fun env summary -> add_summary ~full_free_names_of_module_path ~add_simple env summary)

let add_external_summaries =
  add_summaries ~full_free_names_of_module_path:module_path_root_free_names ~add_simple:false

let with_summaries =
  add_summaries ~full_free_names_of_module_path:module_path_root_free_names ~add_simple:true

let binding_exports_from_provider = fun provider ->
  List.fold_left
    provider.exports
    ~init:Dep_env.empty
    ~fn:(fun exports path -> Dep_env.add_path exports ~path ~free_names:provider.free_names)

let add_provider = fun env provider ->
  Dep_env.add_binding
    env
    ~path:provider.path
    ~free_names:provider.free_names
    ~exports:(binding_exports_from_provider provider)

let make_env = fun providers -> List.fold_left providers ~init:Dep_env.empty ~fn:add_provider

let add_providers = fun env providers -> List.fold_left providers ~init:env ~fn:add_provider

let resolve_summary = fun env (summary: Ir.source_summary) ->
  match Ast_deps.from_parse_result ~env summary with
  | Error _ as error -> error
  | Ok (deps, _env, _exports) ->
      let modules = DepSet.elements deps in
      let modules =
        match summary.module_path with
        | Some (root :: _) -> List.filter modules ~fn:(fun name -> not (String.equal name root))
        | Some []
        | None -> modules
      in
      Ok (Resolution.make ~modules ~unresolved:[])

let resolve = fun env (summaries: Ir.source_summary list) ->
  let env = with_summaries env summaries in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | summary :: rest ->
        match resolve_summary env summary with
        | Error _ as error -> error
        | Ok resolution ->
            loop
              ({
                ResolvedSource.source = summary.source;
                source_hash = summary.source_hash;
                module_path = summary.module_path;
                modules = Resolution.modules resolution;
                unresolved = Resolution.unresolved resolution;
              } :: acc)
              rest
  in
  loop [] summaries
