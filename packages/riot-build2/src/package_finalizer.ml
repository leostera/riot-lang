open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap
module HashMap = Collections.HashMap
module HashSet = Collections.HashSet

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  package_planning: Package_planning.t;
  module_planning: Module_planning.t;
  action_executor: Action_executor.t;
  package_results: (Goal.build_package, Build_result.package_result) ConcurrentHashMap.t;
  registered_actions: (Goal.build_package, unit) ConcurrentHashMap.t;
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
    package_results = ConcurrentHashMap.with_capacity ~size:128;
    registered_actions = ConcurrentHashMap.with_capacity ~size:128;
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

let action_dependency_key = fun action_ref -> Work_node.ActionExecutionKey action_ref

let action_ref = fun (plan: Module_plan.t) node ->
  Action_execution.ref_from_action
    ~package:plan.package.name
    ~profile:plan.profile
    ~target:plan.target
    node

let register_action_nodes = fun t registry (plan: Module_plan.t) ->
  let refs_by_id = HashMap.create () in
  List.for_each
    plan.action_nodes
    ~fn:(fun action ->
      let ref_ = action_ref plan action in
      ignore (HashMap.insert refs_by_id ~key:(Riot_planner.Action_node.id action) ~value:ref_));
  List.map
    plan.action_nodes
    ~fn:(fun action ->
      let ref_ =
        HashMap.get refs_by_id ~key:(Riot_planner.Action_node.id action)
        |> Option.expect ~msg:"action ref should have been registered"
      in
      let dependencies =
        Riot_planner.Action_node.deps action
        |> List.filter_map ~fn:(fun dep_id -> HashMap.get refs_by_id ~key:dep_id)
      in
      let action_execution = {
        Action_execution.ref_;
        action;
        dependencies;
        sandbox_dir = plan.sandbox_dir;
      }
      in
      ignore (Work_registry.intern_action_execution registry action_execution);
      ref_)

let compute_export_entries = fun t (plan: Module_plan.t) ->
  plan.action_nodes
  |> List.flat_map
    ~fn:(fun node ->
      let is_package_export =
        List.any
          (Riot_planner.Action_node.value node).actions
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Riot_planner.Action.CreateLibrary _
            | Riot_planner.Action.CreateExecutable _
            | Riot_planner.Action.CreateSharedLibrary _ -> true
            | CompileInterface _
            | CompileImplementation _
            | GenerateInterface _
            | CompileC _
            | CopyFile _
            | WriteFile _
            | BuildForeignDependency _ -> false)
      in
      if not is_package_export then
        []
      else
        let ref_ = action_ref plan node in
        match Action_executor.artifact t.action_executor ref_ with
        | None -> []
        | Some artifact ->
            let action_hash = Crypto.Digest.hex artifact.Riot_store.Artifact.input_hash in
            List.map
              (Riot_planner.Action_node.value node).outs
              ~fn:(fun out_path ->
                Riot_store.Store.{ name = Path.basename out_path; path = out_path; action_hash }))

let collect_package_outputs = fun (plan: Module_plan.t) ->
  let seen = HashSet.create () in
  plan.action_nodes
  |> List.flat_map ~fn:(fun node -> (Riot_planner.Action_node.value node).outs)
  |> List.filter_map
    ~fn:(fun out ->
      let abs = Path.join plan.sandbox_dir out in
      if HashSet.insert seen ~value:(Path.to_string abs) then
        Some abs
      else
        None)

let missing_action_results = fun t (plan: Module_plan.t) ->
  plan.action_nodes
  |> List.filter_map
    ~fn:(fun node ->
      let ref_ = action_ref plan node in
      match Action_executor.find_result t.action_executor ref_ with
      | Some _ -> None
      | None -> Some ref_)

let finalize = fun t (plan: Module_plan.t) ->
  let failed =
    plan.action_nodes
    |> List.filter_map
      ~fn:(fun node -> Action_executor.failure t.action_executor (action_ref plan node))
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
            let outputs = collect_package_outputs plan in
            let warnings =
              plan.action_nodes
              |> List.filter_map
                ~fn:(fun node ->
                  Action_executor.find_result
                    t.action_executor
                    (action_ref plan node))
              |> List.flat_map ~fn:(fun result -> result.Action_execution.ocamlc_warnings)
            in
            match Riot_store.Store.save_package
              t.store
              ~package:(Riot_model.Package_name.to_string plan.package.name)
              ~ocamlc_warnings:warnings
              ~exports
              ~input_hash:plan.package_hash
              ~sandbox_dir:plan.sandbox_dir
              ~outs:outputs with
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
  Package_planning.dependency_builds t.package_planning build
  |> Result.map
    ~fn:(fun builds ->
      List.map
        builds
        ~fn:(fun build -> Work_node.GoalKey (Goal.BuildPackage build)))

let has_results t goal =
  ConcurrentHashMap.get t.package_results ~key:goal
  |> Option.is_some

let package_dependencies_ready = fun t build ->
  let* dependency_builds = Package_planning.dependency_builds t.package_planning build in
  Ok (List.all dependency_builds ~fn:(has_results t))

let register_action_dependency_keys = fun t registry (plan: Module_plan.t) ->
  let registered =
    ConcurrentHashMap.compute
      t.registered_actions
      ~key:plan.build
      ~fn:(fun current ->
        match current with
        | Some () -> ConcurrentHashMap.Abort true
        | None -> ConcurrentHashMap.Insert ((), false))
  in
  let action_refs =
    if registered then
      List.map plan.action_nodes ~fn:(action_ref plan)
    else
      register_action_nodes t registry plan
  in
  action_refs
  |> List.filter
    ~fn:(fun ref_ ->
      match Action_executor.find_result t.action_executor ref_ with
      | Some _ -> false
      | None -> true)
  |> List.map ~fn:action_dependency_key

let plan_after_package_dependencies = fun t registry build ->
  match Module_planning.find t.module_planning build with
  | None -> Ok [ Work_node.ModulePlanKey build ]
  | Some plan -> Ok (register_action_dependency_keys t registry plan)

let plan_dependencies = fun t registry build ->
  if has_results t build then
    Ok []
  else
    let* package_dependency_keys = package_dependency_goal_keys t build in
    let toolchain_dependency_keys =
      if Package_planning.toolchain_ready t.package_planning build.target then
        []
      else
        [ Work_node.ToolchainReadyKey { target = build.target } ]
    in
    let* package_dependencies_ready = package_dependencies_ready t build in
    if (not package_dependencies_ready) || not (List.is_empty toolchain_dependency_keys) then
      Ok (toolchain_dependency_keys @ package_dependency_keys)
    else
      match Package_planning.cached_artifact t.package_planning build with
      | Error error -> Error error
      | Ok (Some _) -> Ok []
      | Ok None -> plan_after_package_dependencies t registry build

let execute = fun t _registry (build: Goal.build_package) ->
  if has_results t build then
    Ok (Work_result.Complete [])
  else if not (Package_planning.toolchain_ready t.package_planning build.target) then
    Error (Error.ExecutorInvariantViolated {
      message = "package finalization started before toolchain dependency completed";
    })
  else
    match Package_planning.cached_artifact t.package_planning build with
    | Error error -> Error error
    | Ok (Some cached) -> finalize_cached_artifact t cached
    | Ok None ->
        let* package_dependencies_ready = package_dependencies_ready t build in
        if not package_dependencies_ready then
          Error (Error.ExecutorInvariantViolated {
            message = "package finalization started before package dependencies completed";
          })
        else
          match Module_planning.find t.module_planning build with
          | None ->
              Error (Error.ExecutorInvariantViolated {
                message = "package finalization started before module planning completed";
              })
          | Some plan -> finalize t plan
