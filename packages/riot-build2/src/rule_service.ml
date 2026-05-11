open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  package_planning: Package_planning.t;
  package_sandbox: Package_sandbox.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  rule_indexes: (Goal.build_package, rule_index) ConcurrentHashMap.t;
  package_results: (Goal.build_package, Build_result.package_result) ConcurrentHashMap.t;
}

and rule_index = {
  actions_by_ref: (Action_execution.ref_, Action_execution.t) ConcurrentHashMap.t;
  action_keys_by_ref: (Action_execution.ref_, Work_node.key) ConcurrentHashMap.t;
  ocaml_sources_by_path: (Path.t, Rule.ocaml_source) ConcurrentHashMap.t;
  ocaml_actions_by_path_and_role:
    (Path.t * ocaml_action_role, Action_execution.t) ConcurrentHashMap.t;
  c_objects_by_source: (Path.t, Rule.c_object) ConcurrentHashMap.t;
  c_actions_by_source: (Path.t, Action_execution.t) ConcurrentHashMap.t;
}

and ocaml_action_role =
  | Interface
  | ByteImplementation
  | NativeImplementation

let create = fun
  ~workspace
  ~catalog
  ~store
  ~package_planning
  ~package_sandbox
  ~module_planning
  ~action_executor
  () ->
  {
    workspace;
    catalog;
    store;
    package_planning;
    package_sandbox;
    module_planning;
    action_executor;
    rule_indexes = ConcurrentHashMap.with_capacity ~size:128;
    package_results = ConcurrentHashMap.with_capacity ~size:128;
  }

let begin_execution = fun t ->
  ConcurrentHashMap.clear t.rule_indexes;
  ConcurrentHashMap.clear t.package_results

let package_results = fun t ->
  ConcurrentHashMap.values t.package_results
  |> List.sort
    ~compare:(fun left right ->
      let package_compare =
        Riot_model.Package_name.compare left.Build_result.package right.package
      in
      if package_compare != Order.EQ then
        package_compare
      else
        Riot_model.Target.compare left.target right.target)

let find_package_result = fun t build -> ConcurrentHashMap.get t.package_results ~key:build

let store_error = fun ?package reason -> Error.StoreFailed { package; reason }

let ocaml_source_path = fun (source: Rule.ocaml_source) -> source.source.path

let package_dependency_goal_keys = fun t (build: Goal.build_package) ->
  Package_catalog.dependency_names_for_scope
    t.catalog
    ~scope:(Goal.dependency_scope build.scope)
    build.package
  |> Result.map
    ~fn:(fun packages ->
      List.map
        packages
        ~fn:(fun package ->
          Work_node.GoalKey (
            Goal.BuildPackage {
              package;
              scope = build.scope;
              profile = build.profile;
              target = build.target;
            }
          )))

let plan_goal_dependencies = fun t build ->
  let* package_dependency_keys = package_dependency_goal_keys t build in
  Ok (Work_request.from_keys (package_dependency_keys @ [ Work_node.PackageArtifactKey build ]))

let plan_package_artifact_dependencies = fun t build ->
  package_dependency_goal_keys t build
  |> Result.map ~fn:Work_request.from_keys

let toolchain_request = fun target ->
  Work_request.existing (Work_node.ToolchainReadyKey { target })

let source_analysis_requests = fun t build ->
  Module_planning.source_analysis_sources t.module_planning build
  |> Result.map
    ~fn:(fun sources ->
      List.map
        sources
        ~fn:(fun source -> Work_request.materialize (Work_node.SourceAnalysis source)))

let plan_module_dependencies = fun t build -> source_analysis_requests t build

let ensure_module_plan = fun t registry build ->
  match Module_planning.find t.module_planning build with
  | Some plan -> Ok plan
  | None ->
      let* result = Module_planning.execute t.module_planning registry build in
      match result with
      | Work_result.Complete _
      | RequeueWithDependencies _ -> (
          match Module_planning.find t.module_planning build with
          | Some plan -> Ok plan
          | None ->
              Error (Error.ExecutorInvariantViolated {
                message = "module dependency rule completed without a materialized module plan for "
                ^ Riot_model.Package_name.to_string build.package;
              })
        )

let execute_module_dependencies = fun t registry build ->
  let* _plan = ensure_module_plan t registry build in
  Ok (Work_result.Complete [])

let is_final_archive_action = fun __tmp1 ->
  match __tmp1 with
  | Action.CompileLibrary { sources = []; outputs; _ } ->
      List.any outputs ~fn:(fun output -> Path.extension output = Some ".cmxa")
  | _ -> false

let create_rule_index = fun t (plan: Module_plan.t) ->
  let* ocaml_sources = Module_planning.ocaml_sources t.module_planning plan.build in
  let* c_objects = Module_planning.c_objects t.module_planning plan.build in
  let source_capacity = Int.max 16 (List.length ocaml_sources) in
  let c_capacity = Int.max 16 (List.length c_objects) in
  let index = {
    actions_by_ref = ConcurrentHashMap.with_capacity ~size:(List.length plan.action_executions);
    action_keys_by_ref = ConcurrentHashMap.with_capacity ~size:(List.length plan.action_executions);
    ocaml_sources_by_path = ConcurrentHashMap.with_capacity ~size:source_capacity;
    ocaml_actions_by_path_and_role = ConcurrentHashMap.with_capacity ~size:(source_capacity * 2);
    c_objects_by_source = ConcurrentHashMap.with_capacity ~size:c_capacity;
    c_actions_by_source = ConcurrentHashMap.with_capacity ~size:c_capacity;
  }
  in
  List.for_each
    ocaml_sources
    ~fn:(fun source ->
      ignore (
        ConcurrentHashMap.insert
          index.ocaml_sources_by_path
          ~key:(ocaml_source_path source)
          ~value:source
      ));
  List.for_each
    c_objects
    ~fn:(fun c_object ->
      ignore (
        ConcurrentHashMap.insert
          index.c_objects_by_source
          ~key:c_object.Rule.source
          ~value:c_object
      ));
  List.for_each
    plan.action_executions
    ~fn:(fun (action: Action_execution.t) ->
      ignore (ConcurrentHashMap.insert index.actions_by_ref ~key:action.ref_ ~value:action);
      match action.action with
      | Action.CompileSource { source; _ } ->
          ignore (
            ConcurrentHashMap.insert
              index.ocaml_actions_by_path_and_role
              ~key:(
                source.source,
                match source.kind with
                | Action.LibraryInterface -> Interface
                | Action.LibraryImplementation -> NativeImplementation
              )
              ~value:action
          )
      | Action.CompileInterface { source; _ } ->
          ignore (
            ConcurrentHashMap.insert
              index.ocaml_actions_by_path_and_role
              ~key:(source.source, Interface)
              ~value:action
          )
      | Action.CompileByteImplementation { source; _ } ->
          ignore (
            ConcurrentHashMap.insert
              index.ocaml_actions_by_path_and_role
              ~key:(source.source, ByteImplementation)
              ~value:action
          )
      | Action.CompileNativeImplementation { source; _ } ->
          ignore (
            ConcurrentHashMap.insert
              index.ocaml_actions_by_path_and_role
              ~key:(source.source, NativeImplementation)
              ~value:action
          )
      | Action.CompileC { source; _ } ->
          ignore (
            ConcurrentHashMap.insert index.c_actions_by_source ~key:source ~value:action
          )
      | CompileSources _
      | CompileLibrary _
      | CopyFile _
      | WriteFile _ -> ());
  Ok index

let rule_index = fun t (plan: Module_plan.t) ->
  match ConcurrentHashMap.get t.rule_indexes ~key:plan.build with
  | Some index -> Ok index
  | None ->
      let* index = create_rule_index t plan in
      ignore (ConcurrentHashMap.insert t.rule_indexes ~key:plan.build ~value:index);
      Ok index

let source_rule_by_path = fun t plan source_path ->
  let* index = rule_index t plan in
  match ConcurrentHashMap.get index.ocaml_sources_by_path ~key:source_path with
  | Some source -> Ok source
  | None ->
      Error (Error.ExecutorInvariantViolated {
        message = "no OCaml rule found for source " ^ Path.to_string source_path;
      })

let c_object_rule_by_source = fun t plan source_path ->
  let* index = rule_index t plan in
  match ConcurrentHashMap.get index.c_objects_by_source ~key:source_path with
  | Some c_object -> Ok c_object
  | None ->
      Error (Error.ExecutorInvariantViolated {
        message = "no C object rule found for source " ^ Path.to_string source_path;
      })

let action_key = fun t (plan: Module_plan.t) (action: Action_execution.t) ->
  let* index = rule_index t plan in
  match ConcurrentHashMap.get index.action_keys_by_ref ~key:action.ref_ with
  | Some key -> Ok key
  | None ->
      let* key =
  match action.action with
  | Action.CompileSource { source = { content = Some _; _ }; _ } ->
      Ok (Work_node.OCamlGeneratedKey (Rule.ocaml_generated ~build:plan.build action))
  | Action.CompileInterface { source = { content = Some _; _ }; _ }
  | Action.CompileByteImplementation { source = { content = Some _; _ }; _ }
  | Action.CompileNativeImplementation { source = { content = Some _; _ }; _ } ->
      Ok (Work_node.OCamlGeneratedKey (Rule.ocaml_generated ~build:plan.build action))
  | Action.CompileSource { source = { source; kind = Action.LibraryInterface; _ }; _ } ->
      let* rule = source_rule_by_path t plan source in
      Ok (Work_node.OCamlInterfaceKey rule)
  | Action.CompileSource { source = { source; kind = Action.LibraryImplementation; _ }; _ } ->
      let* rule = source_rule_by_path t plan source in
      Ok (Work_node.OCamlImplementationKey rule)
  | Action.CompileInterface { source = { source; _ }; _ } ->
      let* rule = source_rule_by_path t plan source in
      Ok (Work_node.OCamlInterfaceKey rule)
  | Action.CompileByteImplementation { source = { source; _ }; _ } ->
      let* rule = source_rule_by_path t plan source in
      Ok (Work_node.OCamlByteImplementationKey rule)
  | Action.CompileNativeImplementation { source = { source; _ }; _ } ->
      let* rule = source_rule_by_path t plan source in
      Ok (Work_node.OCamlImplementationKey rule)
  | Action.CompileC { source; _ } ->
      let* rule = c_object_rule_by_source t plan source in
      Ok (Work_node.CObjectKey rule)
  | Action.CompileLibrary _ when is_final_archive_action action.action ->
      Ok (Work_node.OCamlArchiveKey plan.build)
  | Action.CompileSources _
  | Action.CompileLibrary _
  | Action.CopyFile _
  | Action.WriteFile _ -> Ok (Work_node.ActionExecutionKey action.ref_)
      in
      ignore (ConcurrentHashMap.insert index.action_keys_by_ref ~key:action.ref_ ~value:key);
      Ok key

let action_by_ref = fun t plan ref_ ->
  let* index = rule_index t plan in
  Ok (ConcurrentHashMap.get index.actions_by_ref ~key:ref_)

let dependency_requests_for_refs = fun t plan refs ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | ref_ :: rest -> (
        match action_by_ref t plan ref_ with
        | Error _ as error -> error
        | Ok None ->
            Error (Error.ExecutorInvariantViolated {
              message = "rule dependency "
              ^ Crypto.Digest.hex ref_.Action_execution.hash
              ^ " was not present in the package action plan";
            })
        | Ok (Some dependency_action) ->
            let* key = action_key t plan dependency_action in
            loop (Work_request.existing key :: acc) rest
      )
  in
  loop [] refs

let dependency_requests_for_action = fun t plan action ->
  dependency_requests_for_refs t plan action.Action_execution.dependencies

let find_archive_action = fun (plan: Module_plan.t) ->
  match plan.ocaml_archive with
  | Some action -> Ok action
  | None ->
      Error (Error.ExecutorInvariantViolated {
        message = "package "
        ^ Riot_model.Package_name.to_string plan.package.name
        ^ " has no OCaml archive action";
      })

let find_ocaml_action = fun t (plan: Module_plan.t) (source: Rule.ocaml_source) expected_role ->
  let source_path = ocaml_source_path source in
  let* source_rule = source_rule_by_path t plan source_path in
  let same_source = Path.equal (ocaml_source_path source_rule) source_path in
  if not same_source then
    Error (Error.ExecutorInvariantViolated { message = "OCaml source rule mismatch"; })
  else
    let* index = rule_index t plan in
    match
      ConcurrentHashMap.get
        index.ocaml_actions_by_path_and_role
        ~key:(source.source.path, expected_role)
    with
    | Some action -> Ok action
    | None ->
        Error (Error.ExecutorInvariantViolated {
          message = "no compiler action found for OCaml source "
          ^ Path.to_string source.source.path;
        })

let find_c_action = fun t (plan: Module_plan.t) c_object ->
  let* index = rule_index t plan in
  match ConcurrentHashMap.get index.c_actions_by_source ~key:c_object.Rule.source with
  | Some action -> Ok action
  | None ->
      Error (Error.ExecutorInvariantViolated {
        message = "no C compiler action found for source "
        ^ Path.to_string c_object.Rule.source;
      })

let missing_dependency_requests = fun t plan action ->
  action.Action_execution.dependencies
  |> List.filter
    ~fn:(fun ref_ -> Option.is_none (Action_executor.find_result t.action_executor ref_))
  |> dependency_requests_for_refs t plan

let execute_action_rule = fun t plan action ->
  let* missing = missing_dependency_requests t plan action in
  match missing with
  | [] -> Action_executor.execute t.action_executor action
  | _ -> Ok (Work_result.RequeueWithDependencies missing)

let plan_ocaml_source = fun _t (source: Rule.ocaml_source) ->
  Ok [
    toolchain_request source.Rule.build.target;
    Work_request.existing (Work_node.ModuleDependenciesKey source.build);
  ]

let plan_ocaml_generated = fun _t (source: Rule.ocaml_generated) ->
  Ok [
    toolchain_request source.Rule.build.target;
    Work_request.existing (Work_node.ModuleDependenciesKey source.build);
  ]

let execute_ocaml_interface = fun t registry (source: Rule.ocaml_source) ->
  let* plan = ensure_module_plan t registry source.Rule.build in
  let* action = find_ocaml_action t plan source Interface in
  execute_action_rule t plan action

let execute_ocaml_byte_implementation = fun t registry (source: Rule.ocaml_source) ->
  let* plan = ensure_module_plan t registry source.Rule.build in
  let* action = find_ocaml_action t plan source ByteImplementation in
  execute_action_rule t plan action

let execute_ocaml_implementation = fun t registry (source: Rule.ocaml_source) ->
  let* plan = ensure_module_plan t registry source.Rule.build in
  let* action = find_ocaml_action t plan source NativeImplementation in
  execute_action_rule t plan action

let execute_ocaml_generated = fun t registry (source: Rule.ocaml_generated) ->
  let* plan = ensure_module_plan t registry source.Rule.build in
  execute_action_rule t plan source.action

let plan_c_object = fun _t (c_object: Rule.c_object) ->
  Ok [
    toolchain_request c_object.build.target;
    Work_request.existing (Work_node.ModuleDependenciesKey c_object.build);
  ]

let execute_c_object = fun t registry (c_object: Rule.c_object) ->
  let* plan = ensure_module_plan t registry c_object.Rule.build in
  let* action = find_c_action t plan c_object in
  execute_action_rule t plan action

let plan_ocaml_archive = fun t build ->
  let* ocaml_sources = Module_planning.ocaml_sources t.module_planning build in
  let* c_objects = Module_planning.c_objects t.module_planning build in
  let ocaml_requests =
    ocaml_sources
    |> List.filter_map
      ~fn:(fun (source: Rule.ocaml_source) ->
        let source_path = ocaml_source_path source in
        if Path.extension source_path = Some ".mli" then
          Some (Work_request.existing (Work_node.OCamlInterfaceKey source))
        else if Path.extension source_path = Some ".ml" then
          Some (Work_request.existing (Work_node.OCamlImplementationKey source))
        else
          None)
  in
  let c_requests =
    c_objects
    |> List.map ~fn:(fun c_object -> Work_request.existing (Work_node.CObjectKey c_object))
  in
  Ok (
    toolchain_request build.target
    :: Work_request.existing (Work_node.ModuleDependenciesKey build)
    :: c_requests
    @ ocaml_requests
  )

let execute_ocaml_archive = fun t registry build ->
  let* plan = ensure_module_plan t registry build in
  let* action = find_archive_action plan in
  execute_action_rule t plan action

let compute_export_entries = fun t (plan: Module_plan.t) ->
  plan.action_executions
  |> List.flat_map
    ~fn:(fun (action: Action_execution.t) ->
      let export_outputs = Action.export_outputs action.Action_execution.action in
      if List.is_empty export_outputs then
        []
      else
        match Action_executor.artifact t.action_executor action.Action_execution.ref_ with
        | None -> []
        | Some artifact ->
            let action_hash = Crypto.Digest.hex artifact.Riot_store.Artifact.input_hash in
            List.map
              export_outputs
              ~fn:(fun out_path ->
                Riot_store.Store.{ name = Path.basename out_path; path = out_path; action_hash }))

let missing_action_results = fun t (plan: Module_plan.t) ->
  plan.action_executions
  |> List.filter_map
    ~fn:(fun (action: Action_execution.t) ->
      match Action_executor.find_result t.action_executor action.Action_execution.ref_ with
      | Some _ -> None
      | None -> Some action.Action_execution.ref_)

let action_artifacts = fun t (plan: Module_plan.t) ->
  plan.action_executions
  |> List.filter_map
    ~fn:(fun (action: Action_execution.t) ->
      Action_executor.artifact t.action_executor action.Action_execution.ref_)

let finalize = fun t (plan: Module_plan.t) ->
  let failed =
    plan.action_executions
    |> List.filter_map
      ~fn:(fun (action: Action_execution.t) ->
        Action_executor.failure t.action_executor action.Action_execution.ref_)
  in
  match failed with
  | reason :: _ -> Error (Error.ActionExecutionFailed { package = plan.package.name; reason })
  | [] ->
      let missing = missing_action_results t plan in
      if not (List.is_empty missing) then
        Error (Error.ExecutorInvariantViolated {
          message = "package artifact rule for "
          ^ Riot_model.Package_name.to_string plan.package.name
          ^ " started before compiler rule dependencies completed";
        })
      else
        let exports = compute_export_entries t plan in
        let target_dir =
          Path.(Riot_model.Riot_dirs.out_dir_in_workspace
            ~workspace:t.workspace
            ~profile:plan.profile.name
            ~target:plan.target
          / Path.v (Riot_model.Package_name.to_string plan.package.name))
        in
        match Riot_store.Store.materialize_package_exports t.store ~exports ~target_dir with
        | Error error ->
            Error (store_error ~package:plan.package.name (Riot_store.Store.error_message error))
        | Ok () ->
            let artifacts = action_artifacts t plan in
            let warnings =
              plan.action_executions
              |> List.filter_map
                ~fn:(fun (action: Action_execution.t) ->
                  Action_executor.find_result t.action_executor action.Action_execution.ref_)
              |> List.flat_map ~fn:(fun result -> result.Action_execution.ocamlc_warnings)
            in
            match Riot_store.Store.save_package_from_action_artifacts
              t.store
              ~package:(Riot_model.Package_name.to_string plan.package.name)
              ~ocamlc_warnings:warnings
              ~exports
              ~input_hash:plan.package_hash
              ~artifacts with
            | Error error ->
                Error (store_error ~package:plan.package.name (Riot_store.Store.error_message error))
            | Ok artifact ->
                let result =
                  Build_result.{
                    package = plan.package.name;
                    profile = plan.profile;
                    target = plan.target;
                    status = Built artifact;
                    ocamlc_warnings = warnings;
                  }
                in
                ignore (ConcurrentHashMap.insert t.package_results ~key:plan.build ~value:result);
                Package_sandbox.cleanup_success t.package_sandbox plan.build;
                Ok (Work_result.Complete [])

let finalize_cached_artifact = fun t (cached: Package_planning.artifact_hit) ->
  let result =
    Build_result.{
      package = cached.package.name;
      profile = cached.profile;
      target = cached.target;
      status = Cached cached.artifact;
      ocamlc_warnings = cached.artifact.Riot_store.Artifact.ocamlc_warnings;
    }
  in
  ignore (ConcurrentHashMap.insert t.package_results ~key:cached.build ~value:result);
  Ok (Work_result.Complete [])

let execute_package_artifact = fun t registry build ->
  match find_package_result t build with
  | Some _ -> Ok (Work_result.Complete [])
  | None -> (
      match Package_planning.cached_artifact t.package_planning build with
      | Error error -> Error error
      | Ok (Some cached) -> finalize_cached_artifact t cached
      | Ok None -> (
          match Module_planning.find t.module_planning build with
          | None ->
              Ok (Work_result.RequeueWithDependencies [
                Work_request.existing (Work_node.OCamlArchiveKey build);
              ])
          | Some plan ->
              let* archive = find_archive_action plan in
              match Action_executor.find_result t.action_executor archive.ref_ with
              | None ->
                  Ok (Work_result.RequeueWithDependencies [
                    Work_request.existing (Work_node.OCamlArchiveKey build);
                  ])
              | Some _ -> finalize t plan
        )
    )
