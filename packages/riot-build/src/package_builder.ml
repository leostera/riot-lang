open Std
open Std.Collections
open Std.Time
open Riot_model
open Riot_planner
open Telemetry_events

type package_error = Telemetry_events.package_error =
  | PlanningFailed of Riot_planner.Planning_error.t
  | ExecutionFailed of { message: string }
  | ActionExecutionFailed of { message: string }
  | ActionOutputsNotCreated of {
      missing: Path.t list;
    }
  | ActionDependenciesFailed of {
      failed: Graph.SimpleGraph.Node_id.t list;
    }

let convert_action_error = fun __tmp1 ->
  match __tmp1 with
  | Action_scheduler.ExecutionFailed { message } ->
      Telemetry_events.ActionExecutionFailed { message }
  | Action_scheduler.OutputsNotCreated { missing } ->
      Telemetry_events.ActionOutputsNotCreated { missing }
  | Action_scheduler.DependenciesFailed { failed } ->
      Telemetry_events.ActionDependenciesFailed { failed }

let package_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | PlanningFailed err -> "Planning failed: " ^ Planning_error.to_string err
  | ExecutionFailed { message } -> "Execution failed: " ^ message ^ ""
  | ActionExecutionFailed { message } -> "Action failed: " ^ message ^ ""
  | ActionOutputsNotCreated { missing } ->
      "Outputs not created: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | ActionDependenciesFailed { failed } ->
      "Dependencies failed: " ^ Int.to_string (List.length failed) ^ " actions"

type build_status =
  | Cached of Riot_store.Artifact.t
  | Built of Riot_store.Artifact.t
  | Skipped of { reason: string }
  | Failed of package_error

type build_result = {
  unit_key: Build_unit.key;
  package: Package.t;
  status: build_status;
  depset: Dependency.t list;
  ocamlc_warnings: string list;
  duration: Duration.t;
}

type detailed_result = {
  result: build_result;
}

type execution_plan = {
  unit_key: Build_unit.key;
  package: Package.t;
  module_graph: Module_node.t Graph.SimpleGraph.t;
  action_graph: Action_graph.t;
  hash: Std.Crypto.hash;
  depset: Dependency.t list;
  started_at: Instant.t;
  emit_visible_progress: bool;
}

type plan_outcome =
  | Final_result of detailed_result
  | Execution_required of execution_plan

let collect_source_files = fun package ->
  let src_dir = Path.(package.Package.path / Path.v "src") in
  match Fs.read_dir src_dir with
  | Error _ -> []
  | Ok reader ->
      let all_files = Std.Iter.MutIterator.to_list reader in
      List.filter_map
        all_files
        ~fn:(fun file_path ->
          let path_str = Path.to_string file_path in
          if
            String.ends_with ~suffix:".ml" path_str
            || String.ends_with ~suffix:".mli" path_str
            || String.ends_with ~suffix:".c" path_str
            || String.ends_with ~suffix:".h" path_str
          then
            let abs_path =
              if String.starts_with ~prefix:"/" path_str then
                file_path
              else
                Path.(src_dir / file_path)
            in
            Some abs_path
          else
            None)

let summarize_package_names = fun names ->
  let rec take_first n acc remaining =
    match (n, remaining) with
    | (0, _) -> (List.reverse acc, remaining)
    | (_, []) -> (List.reverse acc, [])
    | (_, name :: rest) -> take_first (n - 1) (name :: acc) rest
  in
  let (shown, hidden) = take_first 3 [] names in
  let shown_str = String.concat ", " shown in
  match hidden with
  | [] -> shown_str
  | _ ->
      let hidden_count = List.length hidden in
      let hidden_label =
        if hidden_count = 1 then
          "1 more pkg"
        else
          Int.to_string hidden_count ^ " more pkgs"
      in
      shown_str ^ ", and " ^ hidden_label

let action_result_artifact = fun completed (node: Action_node.t) ->
  match HashMap.get completed ~key:(Action_node.id node) with
  | Some {
      Action_executor.status = Action_executor.Cached artifact
      | Action_executor.Executed artifact;
      _;
    } ->
      Some artifact
  | Some _
  | None -> None

let compute_export_entries:
  Action_graph.t ->
  completed:(Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t ->
  Riot_store.Store.export_entry list = fun action_graph ~completed ->
  let entries =
    Action_graph.nodes action_graph
    |> List.flat_map
      ~fn:(fun (node: Action_node.t) ->
        let is_package_export =
          List.any
            (Action_node.value node).actions
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Action.CreateLibrary _
              | Action.CreateExecutable _
              | Action.CreateSharedLibrary _ -> true
              | Action.CompileInterface _
              | Action.CompileImplementation _
              | Action.GenerateInterface _
              | Action.CompileC _
              | Action.CopyFile _
              | Action.WriteFile _
              | Action.BuildForeignDependency _ -> false)
        in
        if not is_package_export then
          []
        else
          match action_result_artifact completed node with
          | None -> []
          | Some artifact ->
              let action_hash_hex = Crypto.Digest.hex artifact.Riot_store.Artifact.input_hash in
              List.map
                (Action_node.value node).outs
                ~fn:(fun out_path ->
                  Riot_store.Store.{
                    name = Path.basename out_path;
                    path = out_path;
                    action_hash = action_hash_hex;
                  }))
  in
  let seen = HashSet.create () in
  List.filter_map
    entries
    ~fn:(fun (entry: Riot_store.Store.export_entry) ->
      if HashSet.contains seen ~value:entry.name then
        None
      else
        (
          let _ = HashSet.insert seen ~value:entry.name in
          Some entry
        ))

let collect_package_artifact_outputs = fun ~sandbox_dir ~outputs ->
  let seen = HashSet.create () in
  outputs
  |> List.filter_map
    ~fn:(fun out_path ->
      let abs_path = Path.join sandbox_dir out_path in
      match Path.strip_prefix abs_path ~prefix:sandbox_dir with
      | Ok _ ->
          let abs_path_str = Path.to_string abs_path in
          if HashSet.contains seen ~value:abs_path_str then
            None
          else
            (
              let _ = HashSet.insert seen ~value:abs_path_str in
              Some abs_path
            )
      | Error _ -> None)

let plan_detailed_from_result = fun
  ~start
  ~session_id
  ~build_target
  ~package_name
  ~package_name_string
  ~emit_visible_progress
  ~unit_key
  ~(package:Package.t)
  (plan_result: (Riot_planner.Package_planner.plan_result, Riot_planner.Planning_error.t) result) ->
  match plan_result with
  | Error err ->
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      (* Don't mark as Failed in graph - planning errors don't have a hash *)
      Telemetry.emit
        (
          BuildFailed {
            session_id;
            package;
            target = Package package_name;
            build_target;
            error = PlanningFailed err;
          }
        );
      Final_result {
        result =
          {
            unit_key;
            package;
            status = Failed (PlanningFailed err);
            depset = [];
            ocamlc_warnings = [];
            duration;
          };
      }
  | Ok (
    Riot_planner.Package_planner.Cached {
      unit_key = planned_key;
      artifact;
      depset;
      _;
    }
  ) ->
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      if emit_visible_progress && List.length artifact.ocamlc_warnings > 0 then
        Telemetry.emit
          (
            PackageOcamlcWarnings {
              session_id;
              package;
              target = Package package_name;
              build_target;
              source = `Cached;
              messages = artifact.ocamlc_warnings;
            }
          );
      if emit_visible_progress then
        Telemetry.emit
          (
            BuildCompleted {
              session_id;
              package;
              target = Package package_name;
              build_target;
              status = `Cached;
              duration;
            }
          );
      Final_result {
        result =
          {
            unit_key = planned_key;
            package;
            status = Cached artifact;
            depset;
            ocamlc_warnings = artifact.ocamlc_warnings;
            duration;
          };
      }
  | Ok (
    Planned {
      unit_key = planned_key;
      hash = package_hash;
      depset;
      module_graph;
      action_graph;
      _;
    }
  ) ->
      Execution_required {
        unit_key = planned_key;
        package;
        module_graph;
        action_graph;
        hash = package_hash;
        depset;
        started_at = start;
        emit_visible_progress;
      }

let plan_build_unit = fun
  ~on_source_analyzed
  ~input_hash_cache
  ~workspace ~toolchain ~store ~(unit:Build_unit.t) ~depset ~build_ctx ~emit_visible_progress ->
  let package = Build_unit.package unit in
  let start = Instant.now () in
  let session_id = build_ctx.Build_ctx.session_id in
  let build_target = Build_ctx.target_triplet build_ctx in
  let package_name = package.Package.name in
  let package_name_string = Package_name.to_string package_name in
  Riot_planner.Package_planner.plan_build_unit_with_cache
    ~on_source_analyzed
    ~input_hash_cache
    ~workspace
    ~toolchain
    ~store
    ~unit
    ~depset
    ~build_ctx
  |> plan_detailed_from_result
    ~start
    ~session_id
    ~build_target
    ~package_name
    ~package_name_string
    ~emit_visible_progress
    ~unit_key:(Build_unit.key unit)
    ~package

type prepared_execution = {
  execution_plan: execution_plan;
  sandbox: Sandbox.t;
  toolchain: Riot_toolchain.t;
}

let execution_inputs = fun (execution_plan: execution_plan) ->
  let package = execution_plan.package in
  List.concat [ package.sources.src; package.sources.native; package.sources.tests ]

let execution_outputs = fun (execution_plan: execution_plan) ->
  Action_graph.nodes execution_plan.action_graph
  |> List.flat_map ~fn:(fun (node: Action_node.t) -> (Action_node.value node).outs)

let failed_execution_result = fun
  ~session_id
  ~build_target
  ~(execution_plan:execution_plan)
  ~(error:package_error)
  ~graph_error ->
  let package = execution_plan.package in
  let package_name = package.Package.name in
  let duration = Instant.duration_since ~earlier:execution_plan.started_at (Instant.now ()) in
  Telemetry.emit
    (
      BuildFailed {
        session_id;
        package;
        target = Package package_name;
        build_target;
        error;
      }
    );
  {
    result =
      {
        unit_key = execution_plan.unit_key;
        package;
        status = Failed error;
        depset = execution_plan.depset;
        ocamlc_warnings = [];
        duration;
      };
  }

let prepare_execution = fun ~workspace ~toolchain ~store ~execution_plan ~build_ctx ->
  let session_id = build_ctx.Build_ctx.session_id in
  let profile_name = build_ctx.Build_ctx.profile.name in
  let target_triplet = Build_ctx.target_triplet build_ctx in
  let package = execution_plan.package in
  let package_name = package.Package.name in
  let package_name_string = Package_name.to_string package_name in
  if execution_plan.emit_visible_progress then
    Telemetry.emit
      (
        PackageStarted {
          session_id;
          package;
          target = Package package_name;
          started_at = Instant.now ();
        }
      );
  Log.info ("Package " ^ package_name_string ^ ": executing action graph");
  Log.info
    ("Package "
    ^ package_name_string
    ^ ": executing action graph with "
    ^ Int.to_string (List.length (Action_graph.nodes execution_plan.action_graph))
    ^ " nodes");
  if
    execution_plan.emit_visible_progress
    && List.length (Action_graph.nodes execution_plan.action_graph) > 0
  then
    Telemetry.emit
      (
        CompilationStarted {
          session_id;
          package;
          target = Package package_name;
          build_target = target_triplet;
          action_count = List.length (Action_graph.nodes execution_plan.action_graph);
          started_at = Instant.now ();
        }
      );
  let inputs = execution_inputs execution_plan in
  let prepare_started_at = Instant.now () in
  try
    let sandbox_create_started_at = Instant.now () in
    let sandbox =
      Sandbox.create
        ~workspace
        ~id_seed:execution_plan.hash
        ~session_id
        ~profile:profile_name
        ~target:target_triplet
        ()
        ~package_name:package.Package.name
    in
    if execution_plan.emit_visible_progress then (
      let created_at = Instant.now () in
      Telemetry.emit
        (
          SandboxCreated {
            session_id;
            package;
            target = Package package_name;
            build_target = target_triplet;
            path = Sandbox.get_dir sandbox;
            created_at;
            duration = Instant.duration_since ~earlier:sandbox_create_started_at created_at;
          }
        )
    );
    let input_copy_started_at = Instant.now () in
    let input_count = Sandbox.copy_inputs ~sandbox ~package ~inputs in
    if execution_plan.emit_visible_progress then (
      let copied_at = Instant.now () in
      Telemetry.emit
        (
          SandboxInputsCopied {
            session_id;
            package;
            target = Package package_name;
            build_target = target_triplet;
            input_count;
            copied_at;
            duration = Instant.duration_since ~earlier:input_copy_started_at copied_at;
          }
        )
    );
    let dependency_copy_started_at = Instant.now () in
    match
      Sandbox.copy_dependency_object_files ~store ~sandbox ~package ~depset:execution_plan.depset
    with
    | Error err ->
        let error_msg = Sandbox.dependency_copy_error_to_string err in
        let error = ExecutionFailed { message = error_msg } in
        Sandbox.cleanup sandbox;
        Error (failed_execution_result
          ~session_id
          ~build_target:target_triplet
          ~execution_plan
          ~error
          ~graph_error:error_msg)
    | Ok dependency_stats ->
        if execution_plan.emit_visible_progress then (
          let copied_at = Instant.now () in
          Telemetry.emit
            (
              SandboxDependenciesCopied {
                session_id;
                package;
                target = Package package_name;
                build_target = target_triplet;
                dependency_count = dependency_stats.dependency_count;
                object_count = dependency_stats.object_count;
                copied_at;
                duration = Instant.duration_since ~earlier:dependency_copy_started_at copied_at;
              }
            )
        );
        if execution_plan.emit_visible_progress then (
          let prepared_at = Instant.now () in
          Telemetry.emit
            (
              PackageExecutionPrepared {
                session_id;
                package;
                target = Package package_name;
                build_target = target_triplet;
                input_count;
                dependency_count = dependency_stats.dependency_count;
                dependency_object_count = dependency_stats.object_count;
                prepared_at;
                duration = Instant.duration_since ~earlier:prepare_started_at prepared_at;
              }
            )
        );
        Ok { execution_plan; sandbox; toolchain }
  with
  | exn ->
      let error_msg = "Exception: " ^ Exception.to_string exn in
      let error = ExecutionFailed { message = error_msg } in
      Error (failed_execution_result
        ~session_id
        ~build_target:target_triplet
        ~execution_plan
        ~error
        ~graph_error:error_msg)

let execute_action = fun
  ~store ~(prepared_execution:prepared_execution) ~build_ctx ~completed action ->
  Action_executor.execute_node
    ~completed
    ~store
    ~session_id:build_ctx.Build_ctx.session_id
    ~build_target:(Build_ctx.target_triplet build_ctx)
    prepared_execution.toolchain
    (Sandbox.get_dir prepared_execution.sandbox)
    action

let finalize_execution = fun
  ~workspace ~store ~(prepared_execution:prepared_execution) ~completed ~build_ctx ->
  let execution_plan = prepared_execution.execution_plan in
  let session_id = build_ctx.Build_ctx.session_id in
  let profile_name = build_ctx.Build_ctx.profile.name in
  let target_triplet = Build_ctx.target_triplet build_ctx in
  let package = execution_plan.package in
  let package_name = package.Package.name in
  let package_name_string = Package_name.to_string package_name in
  let target_dir =
    Path.(Riot_model.Riot_dirs.out_dir_in_workspace
      ~workspace
      ~profile:profile_name
      ~target:target_triplet
    / Path.v package_name_string)
  in
  let sandbox_dir = Sandbox.get_dir prepared_execution.sandbox in
  let cleanup_and_return result =
    Sandbox.cleanup prepared_execution.sandbox;
    result
  in
  try
    let action_result =
      Action_scheduler.summarize_completed
        ~action_graph:execution_plan.action_graph
        ~completed_results:completed
    in
    match action_result.Action_scheduler.first_failure with
    | Some first_error ->
        let error = convert_action_error first_error in
        cleanup_and_return
          (failed_execution_result
            ~session_id
            ~build_target:target_triplet
            ~execution_plan
            ~error
            ~graph_error:(package_error_to_string error))
    | None ->
        let outputs = execution_outputs execution_plan in
        let export_entries = compute_export_entries execution_plan.action_graph ~completed in
        let package_outputs = collect_package_artifact_outputs ~sandbox_dir ~outputs in
        let ocamlc_warnings = action_result.Action_scheduler.ocamlc_warnings in
        (
          match Riot_store.Store.materialize_package_exports store ~exports:export_entries ~target_dir with
          | Error store_error ->
              let error_msg =
                "Failed to materialize package exports for "
                ^ package_name_string
                ^ ": "
                ^ Riot_store.Store.error_message store_error
              in
              let error = ExecutionFailed { message = error_msg } in
              cleanup_and_return
                (failed_execution_result
                  ~session_id
                  ~build_target:target_triplet
                  ~execution_plan
                  ~error
                  ~graph_error:error_msg)
          | Ok () -> (
              match
                Riot_store.Store.save_package
                  store
                  ~package:package_name_string
                  ~ocamlc_warnings
                  ~exports:export_entries
                  ~input_hash:execution_plan.hash
                  ~sandbox_dir
                  ~outs:package_outputs
              with
              | Error store_error ->
                  let error_msg =
                    "Failed to save package hash artifact for "
                    ^ package_name_string
                    ^ ": "
                    ^ Riot_store.Store.error_message store_error
                  in
                  let error = ExecutionFailed { message = error_msg } in
                  cleanup_and_return
                    (failed_execution_result
                      ~session_id
                      ~build_target:target_triplet
                      ~execution_plan
                      ~error
                      ~graph_error:error_msg)
              | Ok artifact ->
                  if execution_plan.emit_visible_progress && List.length ocamlc_warnings > 0 then
                    Telemetry.emit
                      (
                        PackageOcamlcWarnings {
                          session_id;
                          package;
                          target = Package package_name;
                          build_target = target_triplet;
                          source = `Fresh;
                          messages = ocamlc_warnings;
                        }
                      );
                  Sandbox.cleanup prepared_execution.sandbox;
                  let duration = Instant.duration_since ~earlier:execution_plan.started_at (Instant.now ()) in
                  if execution_plan.emit_visible_progress then
                    Telemetry.emit
                      (
                        BuildCompleted {
                          session_id;
                          package;
                          target = Package package_name;
                          build_target = target_triplet;
                          status = `Fresh;
                          duration;
                        }
                      );
                  {
                    result =
                      {
                        unit_key = execution_plan.unit_key;
                        package;
                        status = Built artifact;
                        depset = execution_plan.depset;
                        ocamlc_warnings;
                        duration;
                      };
                  }
            )
        )
  with
  | exn ->
      let error_msg = "Exception: " ^ Exception.to_string exn in
      let error = ExecutionFailed { message = error_msg } in
      cleanup_and_return
        (failed_execution_result
          ~session_id
          ~build_target:target_triplet
          ~execution_plan
          ~error
          ~graph_error:error_msg)

let execute_detailed = fun ~workspace ~toolchain ~store ~execution_plan ~build_ctx ->
  let target_triplet = Build_ctx.target_triplet build_ctx in
  match prepare_execution ~workspace ~toolchain ~store ~execution_plan ~build_ctx with
  | Error detailed_result -> detailed_result
  | Ok prepared_execution ->
      let action_result =
        Action_scheduler.run
          ~action_graph:execution_plan.action_graph
          ~sandbox:prepared_execution.sandbox
          ~store
          ~session_id:build_ctx.Build_ctx.session_id
          ~build_target:target_triplet
          prepared_execution.toolchain
          ~concurrency:build_ctx.Build_ctx.parallelism
      in
      let completed: (Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t =
        HashMap.create ()
      in
      List.for_each
        action_result.completed_actions
        ~fn:(fun completed_action ->
          let _ =
            HashMap.insert completed ~key:(Action_node.id completed_action.node) ~value:completed_action.result
          in
          ());
      finalize_execution ~workspace ~store ~prepared_execution ~completed ~build_ctx
