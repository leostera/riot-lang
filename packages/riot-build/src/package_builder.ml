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

let package_error_to_json = fun __tmp1 ->
  match __tmp1 with
  | PlanningFailed planning_err ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "planning_failed");
        ("error", Riot_planner.Planning_error.to_json planning_err);
      ]
  | ExecutionFailed { message } ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "execution_failed");
        ("message", Std.Data.Json.String message);
      ]
  | ActionExecutionFailed { message } ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "action_failed");
        ("message", Std.Data.Json.String message);
      ]
  | ActionOutputsNotCreated { missing } ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "outputs_not_created");
        (
          "missing",
          Std.Data.Json.Array (List.map
            missing
            ~fn:(fun p -> Std.Data.Json.String (Path.to_string p)))
        );
      ]
  | ActionDependenciesFailed { failed } ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "dependencies_failed");
        ("failed_count", Std.Data.Json.String (Int.to_string (List.length failed)));
      ]

type build_status =
  | Cached of Riot_store.Artifact.t
  | Built of Riot_store.Artifact.t
  | Skipped of { reason: string }
  | Failed of package_error

let build_status_to_json = fun __tmp1 ->
  match __tmp1 with
  | Cached artifact ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "cached");
        ("artifact", Riot_store.Artifact.to_json artifact);
      ]
  | Built artifact ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "built");
        ("artifact", Riot_store.Artifact.to_json artifact);
      ]
  | Skipped { reason } ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "skipped");
        ("reason", Std.Data.Json.String reason);
      ]
  | Failed err ->
      Std.Data.Json.Object [
        ("type", Std.Data.Json.String "failed");
        ("error", package_error_to_json err);
      ]

type build_result = {
  package_key: Package.key;
  package: Package.t;
  status: build_status;
  ocamlc_warnings: string list;
  duration: Duration.t;
}

type graph_update =
  | Planned_package of {
      hash: Std.Crypto.hash;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
    }
  | Cached_package of {
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      exports: Riot_store.Store.export_entry list;
    }
  | Built_package of {
      hash: Std.Crypto.hash;
      artifact: Riot_store.Artifact.t;
      depset: Dependency.t list;
      module_graph: Module_node.t Graph.SimpleGraph.t;
      action_graph: Action_graph.t;
      status: Package_graph.build_status;
    }
  | Failed_package of {
      hash: Std.Crypto.hash option;
      error: string;
    }
  | Skipped_package of { reason: string }

type detailed_result = {
  result: build_result;
  graph_update: graph_update option;
}

type execution_plan = {
  package_key: Package.key;
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

let planned_graph_update = fun (execution_plan: execution_plan) ->
  Planned_package {
    hash = execution_plan.hash;
    module_graph = execution_plan.module_graph;
    action_graph = execution_plan.action_graph;
  }

let apply_graph_update = fun package_graph package_key package graph_update ->
  match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
  | None -> ()
  | Some node ->
      let scope = Riot_planner.Package_graph.get_scope node.value in
      match graph_update with
      | None -> ()
      | Some (Planned_package { hash; module_graph; action_graph }) ->
          node.value <- Riot_planner.Package_graph.Planned {
            package;
            scope;
            module_graph;
            action_graph;
            hash;
          }
      | Some (Cached_package {
                hash;
                artifact;
                depset;
                exports;
              }) ->
          node.value <- Riot_planner.Package_graph.Cached {
            package;
            scope;
            hash;
            artifact;
            depset;
            exports;
          }
      | Some (Built_package {
                hash;
                artifact;
                depset;
                module_graph;
                action_graph;
                status;
              }) ->
          node.value <- Riot_planner.Package_graph.Built {
            package;
            scope;
            module_graph;
            action_graph;
            hash;
            artifact;
            status;
            depset;
          }
      | Some (Failed_package { hash = Some hash; error }) ->
          node.value <- Riot_planner.Package_graph.Failed {
            package;
            scope;
            hash;
            error;
          }
      | Some (Failed_package { hash = None; _ }) -> ()
      | Some (Skipped_package { reason }) ->
          node.value <- Riot_planner.Package_graph.Skipped { package; scope; reason }

let build_result_to_json = fun (result: build_result) ->
  Std.Data.Json.Object [
    ("package_key", Std.Data.Json.String (Package.key_to_string result.package_key));
    ("package", Package.to_json result.package);
    ("status", build_status_to_json result.status);
    (
      "ocamlc_warnings",
      Std.Data.Json.Array (List.map result.ocamlc_warnings ~fn:(fun msg -> Std.Data.Json.String msg))
    );
    (
      "duration_ms",
      Std.Data.Json.Int (Int.from_float (Duration.to_secs_float result.duration *. 1_000.0))
    );
  ]

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
  match HashMap.get completed ~key:node.id with
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
            node.value.actions
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
                node.value.outs
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

let package_scope = fun package_graph package_key ->
  match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
  | Some node -> Riot_planner.Package_graph.get_scope node.value
  | None -> Riot_planner.Package_graph.Runtime

let emits_visible_progress = fun __tmp1 ->
  match __tmp1 with
  | Riot_planner.Package_graph.Build -> false
  | Riot_planner.Package_graph.Runtime
  | Riot_planner.Package_graph.Dev -> true

let plan_detailed = fun
  ~workspace ~toolchain ~store ~package_graph ~package_key ~(package:Package.t) ~build_ctx ->
  let start = Instant.now () in
  let session_id = build_ctx.Build_ctx.session_id in
  let build_target = Build_ctx.target_triplet build_ctx in
  let package_name = package.Package.name in
  let package_name_string = Package_name.to_string package_name in
  Log.info ("Package " ^ package_name_string ^ ": computing content hash with dependencies");
  let emit_visible_progress =
    package_scope package_graph package_key
    |> emits_visible_progress
  in
  match Riot_planner.plan_package_with_graph
    ~workspace
    ~toolchain
    ~store
    ~package_graph
    ~package_key
    ~package
    ~build_ctx with
  | Error err ->
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      (* Don't mark as Failed in graph - planning errors don't have a hash *)
      Telemetry.emit
        (
          BuildFailed {
            session_id;
            package;
            target = Workspace_planner.Package package_name;
            build_target;
            error = PlanningFailed err;
          }
        );
      Final_result {
        result =
          {
            package_key;
            package;
            status = Failed (PlanningFailed err);
            ocamlc_warnings = [];
            duration;
          };
        graph_update = None;
      }
  | Ok (MissingDependencies { missing; _ }) ->
      let missing_names = List.map missing ~fn:(fun p -> Package_name.to_string p.Package.name) in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let error = "Missing dependencies: " ^ String.concat ", " missing_names in
      (* Don't mark as Failed - this is a transient planning state *)
      let error_variant = ExecutionFailed { message = error } in
      Telemetry.emit
        (
          BuildFailed {
            session_id;
            package;
            target = Workspace_planner.Package package_name;
            build_target;
            error = error_variant;
          }
        );
      Final_result {
        result =
          {
            package_key;
            package;
            status = Failed error_variant;
            ocamlc_warnings = [];
            duration;
          };
        graph_update = None;
      }
  | Ok (FailedDependencies { failed; _ }) ->
      let failed_names = List.map failed ~fn:(fun p -> Package_name.to_string p.Package.name) in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let reason = "needs " ^ summarize_package_names failed_names in
      Log.info ("Package " ^ package_name_string ^ ": SKIPPED (" ^ reason ^ ")");
      Telemetry.emit
        (
          BuildSkipped {
            session_id;
            package;
            target = Workspace_planner.Package package_name;
            build_target;
            reason;
          }
        );
      Final_result {
        result =
          {
            package_key;
            package;
            status = Skipped { reason };
            ocamlc_warnings = [];
            duration;
          };
        graph_update = Some (Skipped_package { reason });
      }
  | Ok (
    Riot_planner.Package_planner.Cached {
      package_key = planned_key;
      hash = package_hash;
      artifact;
      depset;
      exports;
    }
  ) ->
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      if emit_visible_progress && List.length artifact.ocamlc_warnings > 0 then
        Telemetry.emit
          (
            PackageOcamlcWarnings {
              session_id;
              package;
              target = Workspace_planner.Package package_name;
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
              target = Workspace_planner.Package package_name;
              build_target;
              status = `Cached;
              duration;
            }
          );
      Final_result {
        result =
          {
            package_key = planned_key;
            package;
            status = Cached artifact;
            ocamlc_warnings = artifact.ocamlc_warnings;
            duration;
          };
        graph_update =
          Some (
            Cached_package {
              hash = package_hash;
              artifact;
              depset;
              exports;
            }
          );
      }
  | Ok (
    Planned {
      package_key = planned_key;
      hash = package_hash;
      depset;
      module_graph;
      action_graph;
    }
  ) ->
      (
          Log.info
            ("Package " ^ package_name_string ^ ": hash=" ^ Std.Crypto.Digest.hex package_hash);
          Execution_required {
            package_key = planned_key;
            package;
            module_graph;
            action_graph;
            hash = package_hash;
            depset;
            started_at = start;
            emit_visible_progress;
          }
        )

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
  |> List.flat_map ~fn:(fun (node: Action_node.t) -> node.value.outs)

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
        target = Workspace_planner.Package package_name;
        build_target;
        error;
      }
    );
  {
    result =
      {
        package_key = execution_plan.package_key;
        package;
        status = Failed error;
        ocamlc_warnings = [];
        duration;
      };
    graph_update = Some (Failed_package { hash = Some execution_plan.hash; error = graph_error });
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
      (BuildStarted { session_id; package; target = Workspace_planner.Package package_name });
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
          target = Workspace_planner.Package package_name;
          build_target = target_triplet;
        }
      );
  let inputs = execution_inputs execution_plan in
  try
    let sandbox =
      Sandbox.create
        ~workspace
        ~profile:profile_name
        ~target:target_triplet
        ()
        ~package_name:package.Package.name
    in
    Sandbox.prepare ~sandbox ~package ~inputs ~depset:execution_plan.depset ~store;
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
        Riot_store.Store.materialize_package_exports store ~exports:export_entries ~target_dir
        |> Result.expect ~msg:("Failed to materialize package exports for " ^ package_name_string);
        let artifact =
          Riot_store.Store.save
            store
            ~package:package_name_string
            ~ocamlc_warnings
            ~exports:export_entries
            ~input_hash:execution_plan.hash
            ~sandbox_dir
            ~outs:package_outputs
          |> Result.expect ~msg:("Failed to save package hash artifact for " ^ package_name_string)
        in
        if execution_plan.emit_visible_progress && List.length ocamlc_warnings > 0 then
          Telemetry.emit
            (
              PackageOcamlcWarnings {
                session_id;
                package;
                target = Workspace_planner.Package package_name;
                build_target = target_triplet;
                source = `Fresh;
                messages = ocamlc_warnings;
              }
            );
        let duration = Instant.duration_since ~earlier:execution_plan.started_at (Instant.now ()) in
        if execution_plan.emit_visible_progress then
          Telemetry.emit
            (
              BuildCompleted {
                session_id;
                package;
                target = Workspace_planner.Package package_name;
                build_target = target_triplet;
                status = `Fresh;
                duration;
              }
            );
        cleanup_and_return
          {
            result =
              {
                package_key = execution_plan.package_key;
                package;
                status = Built artifact;
                ocamlc_warnings;
                duration;
              };
            graph_update =
              Some (
                Built_package {
                  hash = execution_plan.hash;
                  artifact;
                  depset = execution_plan.depset;
                  module_graph = execution_plan.module_graph;
                  action_graph = execution_plan.action_graph;
                  status = Riot_planner.Package_graph.Fresh;
                }
              );
          }
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
  match prepare_execution ~workspace ~toolchain ~store ~execution_plan ~build_ctx with
  | Error detailed_result -> detailed_result
  | Ok prepared_execution ->
      let action_result =
        Action_scheduler.run
          ~action_graph:execution_plan.action_graph
          ~sandbox:prepared_execution.sandbox
          ~store
          ~session_id:build_ctx.Build_ctx.session_id
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
            HashMap.insert completed ~key:completed_action.node.id ~value:completed_action.result
          in
          ());
      finalize_execution ~workspace ~store ~prepared_execution ~completed ~build_ctx

let build_detailed = fun
  ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx ->
  match plan_detailed ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx with
  | Final_result detailed_result -> detailed_result
  | Execution_required execution_plan ->
      execute_detailed ~workspace ~toolchain ~store ~execution_plan ~build_ctx

let build = fun ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx ->
  let detailed_result =
    build_detailed ~workspace ~toolchain ~store ~package_graph ~package_key ~package ~build_ctx
  in
  apply_graph_update
    package_graph
    detailed_result.result.package_key
    detailed_result.result.package
    detailed_result.graph_update;
  detailed_result.result
