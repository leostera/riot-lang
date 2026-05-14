open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  package_planning: Package_planning.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  action_plan_cache: Action_plan_cache.payload Graph_cache.t;
  package_results: (Goal.build_package, Build_result.package_result) ConcurrentHashMap.t;
}

let create = fun
  ~workspace ~catalog ~store ~package_planning ~module_planning ~action_executor () ->
  {
    workspace;
    catalog;
    store;
    package_planning;
    module_planning;
    action_executor;
    action_plan_cache = Action_plan_cache.create_cache ~store;
    package_results = ConcurrentHashMap.with_capacity ~size:128;
  }

let results = fun t ->
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

let find = fun t build -> ConcurrentHashMap.get t.package_results ~key:build

let store_error = fun ?package reason -> Error.StoreFailed { package; reason }

let same_action_ref = fun (left: Action_execution.ref_) (right: Action_execution.ref_) ->
  Riot_model.Package_name.equal left.Action_execution.package right.Action_execution.package
  && Riot_model.Target.equal left.target right.target
  && Crypto.Hash.equal left.hash right.hash

let plan_has_library_ref = fun (plan: Module_plan.t) ref_ ->
  List.any
    plan.ocaml_libraries
    ~fn:(fun action -> same_action_ref action.Action_execution.ref_ ref_)

let plan_has_archive_ref = fun (plan: Module_plan.t) ref_ ->
  match plan.ocaml_archive with
  | Some action -> same_action_ref action.Action_execution.ref_ ref_
  | None -> false

let action_dependency_key = fun (plan: Module_plan.t) action_ref ->
  if plan_has_library_ref plan action_ref then
    Work_node.OCamlLibraryKey action_ref
  else
    Work_node.ActionExecutionKey action_ref

let action_node_kind = fun (plan: Module_plan.t) (action: Action_execution.t) ->
  match action_dependency_key plan action.Action_execution.ref_ with
  | Work_node.OCamlLibraryKey _ -> Work_node.OCamlLibrary action
  | Work_node.ActionExecutionKey _ -> Work_node.ActionExecution action
  | _ -> Work_node.ActionExecution action

let action_node_request = fun plan action -> Work_request.materialize (action_node_kind plan action)

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

let completed_action_artifacts = fun t (plan: Module_plan.t) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | (action: Action_execution.t) :: rest -> (
        match Action_executor.artifact t.action_executor action.ref_ with
        | Some artifact -> loop (artifact :: acc) rest
        | None ->
            Error (Error.ExecutorInvariantViolated {
              message = "package finalization for "
              ^ Riot_model.Package_name.to_string plan.package.name
              ^ " could not find completed action artifact";
            })
      )
  in
  loop [] plan.action_executions

let finalize = fun t (plan: Module_plan.t) ->
  let failed =
    plan.action_executions
    |> List.filter_map
      ~fn:(fun (action: Action_execution.t) ->
        Action_executor.failure
          t.action_executor
          action.Action_execution.ref_)
  in
  match failed with
  | reason :: _ -> Error (Error.ActionExecutionFailed { package = plan.package.name; reason })
  | [] ->
      let missing = missing_action_results t plan in
      if not (List.is_empty missing) then
        Error (Error.ExecutorInvariantViolated {
          message = "package finalization for "
          ^ Riot_model.Package_name.to_string plan.package.name
          ^ " started before action execution dependencies completed";
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
            let warnings =
              plan.action_executions
              |> List.filter_map
                ~fn:(fun (action: Action_execution.t) ->
                  Action_executor.find_result
                    t.action_executor
                    action.Action_execution.ref_)
              |> List.flat_map ~fn:(fun result -> result.Action_execution.ocamlc_warnings)
            in
            let* artifacts = completed_action_artifacts t plan in
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

let has_results t goal =
  ConcurrentHashMap.get t.package_results ~key:goal
  |> Option.is_some

let missing_action_dependency_requests = fun
  t (plan: Module_plan.t) (actions: Action_execution.t list) ->
  actions
  |> List.filter
    ~fn:(fun (action: Action_execution.t) ->
      match Action_executor.find_result t.action_executor action.Action_execution.ref_ with
      | Some _ -> false
      | None -> true)
  |> List.map ~fn:(action_node_request plan)

let package_artifact_key = fun build -> Work_node.PackageArtifactKey build

let package_finalize_key = fun build -> Work_node.PackageFinalizeKey build

let action_plan_key = fun build -> Work_node.ActionPlanKey build

let plan_dependencies = fun t _registry build ->
  let* package_dependency_keys = package_dependency_goal_keys t build in
  Ok (Work_request.from_keys (package_dependency_keys @ [ package_artifact_key build ]))

let plan_artifact_dependencies = fun t _registry build ->
  package_dependency_goal_keys t build
  |> Result.map ~fn:Work_request.from_keys

let plan_finalize_dependencies = fun _t _registry build ->
  Ok [ Work_request.existing (action_plan_key build) ]

let plan_action_dependencies = fun _t _registry build ->
  Ok [ Work_request.existing (Work_node.ModulePlanKey build) ]

let execute = fun t _registry (build: Goal.build_package) ->
  if has_results t build then
    Ok (Work_result.Complete [])
  else
    Ok (Work_result.RequeueWithDependencies [ Work_request.existing (package_artifact_key build) ])

let execute_artifact = fun t _registry (build: Goal.build_package) ->
  if has_results t build then
    Ok (Work_result.Complete [])
  else
    match Package_planning.cached_artifact t.package_planning build with
    | Error error -> Error error
    | Ok (Some cached) -> finalize_cached_artifact t cached
    | Ok None ->
        Ok (Work_result.RequeueWithDependencies [
          Work_request.existing (package_finalize_key build);
        ])

let execute_finalize = fun t _registry (build: Goal.build_package) ->
  if has_results t build then
    Ok (Work_result.Complete [])
  else
    match Module_planning.find t.module_planning build with
    | None ->
        Error (Error.ExecutorInvariantViolated {
          message = "package finalization for "
          ^ Riot_model.Package_name.to_string build.package
          ^ " started before module planning completed";
        })
    | Some plan -> finalize t plan

let execute_action_plan = fun t _registry (build: Goal.build_package) ->
  match Module_planning.find t.module_planning build with
  | None ->
      Error (Error.ExecutorInvariantViolated {
        message = "action planning for "
        ^ Riot_model.Package_name.to_string build.package
        ^ " started before module planning completed";
      })
  | Some plan ->
      let* () =
        match Graph_cache.get t.action_plan_cache plan.module_plan_hash with
        | Some (Error error) -> Error error
        | Some (Ok _) -> Ok ()
        | None ->
            Graph_cache.put
              t.action_plan_cache
              plan.module_plan_hash
              (Action_plan_cache.payload_of_plan plan)
      in
      match missing_action_dependency_requests t plan plan.action_executions with
      | [] -> Ok (Work_result.Complete [])
      | missing -> Ok (Work_result.RequeueWithDependencies missing)
