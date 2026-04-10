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
  | ActionOutputsNotCreated of { missing: Path.t list }
  | ActionDependenciesFailed of { failed: Graph.SimpleGraph.Node_id.t list }

let convert_action_error = function
  | Action_executor.ExecutionFailed { message } -> Telemetry_events.ActionExecutionFailed { message }
  | Action_executor.OutputsNotCreated { missing } -> Telemetry_events.ActionOutputsNotCreated {
    missing
  }
  | Action_executor.DependenciesFailed { failed } -> Telemetry_events.ActionDependenciesFailed {
    failed
  }

let package_error_to_string = function
  | PlanningFailed err -> "Planning failed: " ^ Planning_error.to_string err
  | ExecutionFailed { message } -> "Execution failed: " ^ message ^ ""
  | ActionExecutionFailed { message } -> "Action failed: " ^ message ^ ""
  | ActionOutputsNotCreated { missing } -> "Outputs not created: "
  ^ (String.concat ", " (List.map Path.to_string missing))
  | ActionDependenciesFailed { failed } -> "Dependencies failed: "
  ^ Int.to_string (List.length failed)
  ^ " actions"

let package_error_to_json = function
  | PlanningFailed planning_err -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "planning_failed");
    ("error", Riot_planner.Planning_error.to_json planning_err);
  ]
  | ExecutionFailed { message } -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "execution_failed");
    ("message", Std.Data.Json.String message);
  ]
  | ActionExecutionFailed { message } -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "action_failed");
    ("message", Std.Data.Json.String message);
  ]
  | ActionOutputsNotCreated { missing } -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "outputs_not_created");
    (
      "missing",
      Std.Data.Json.Array (List.map (fun p -> Std.Data.Json.String (Path.to_string p)) missing)
    );
  ]
  | ActionDependenciesFailed { failed } -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "dependencies_failed");
    ("failed_count", Std.Data.Json.String (Int.to_string (List.length failed)));
  ]

type build_status =
  | Cached of Riot_store.Artifact.t
  | Built of Riot_store.Artifact.t
  | Skipped of { reason: string }
  | Failed of package_error

let build_status_to_json = function
  | Cached artifact -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "cached");
    ("artifact", Riot_store.Artifact.to_json artifact);
  ]
  | Built artifact -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "built");
    ("artifact", Riot_store.Artifact.to_json artifact);
  ]
  | Skipped { reason } -> Std.Data.Json.Object [
    ("type", Std.Data.Json.String "skipped");
    ("reason", Std.Data.Json.String reason);
  ]
  | Failed err -> Std.Data.Json.Object [
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

let build_result_to_json = fun result ->
  Std.Data.Json.Object [
    ("package_key", Std.Data.Json.String (Package.key_to_string result.package_key));
    ("package", Package.to_json result.package);
    ("status", build_status_to_json result.status);
    (
      "ocamlc_warnings",
      Std.Data.Json.Array (List.map (fun msg -> Std.Data.Json.String msg) result.ocamlc_warnings)
    );
    (
      "duration_ms",
      Std.Data.Json.Int (int_of_float (Duration.to_secs_float result.duration *. 1000.0))
    );
  ]

let collect_source_files = fun package ->
  let src_dir = Path.(package.Package.path / Path.v "src") in
  match Fs.read_dir src_dir with
  | Error _ -> []
  | Ok reader ->
      let all_files = Std.Iter.MutIterator.to_list reader in
      List.filter_map
        (fun file_path ->
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
        all_files

let summarize_package_names = fun names ->
  let rec take_first n acc remaining =
    match (n, remaining) with
    | 0, _ -> (List.rev acc, remaining)
    | _, [] -> (List.rev acc, [])
    | _, name :: rest -> take_first (n - 1) (name :: acc) rest
  in
  let shown, hidden = take_first 3 [] names in
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

let collect_ocamlc_warnings = fun completed_actions ->
  let seen = HashSet.create () in
  HashMap.to_list completed_actions |> List.fold_left
    (fun acc ((_id, result): Graph.SimpleGraph.Node_id.t * Action_executor.execution_result) ->
      List.fold_left
        (fun acc warning ->
          if HashSet.contains seen warning then
            acc
          else
            let _ = HashSet.insert seen warning in
            acc @ [ warning ])
        acc
        result.Action_executor.ocamlc_warnings)
    []

let compute_export_entries: Action_graph.t -> Riot_store.Store.export_entry list = fun action_graph ->
  let entries =
    Action_graph.nodes action_graph
    |> List.concat_map
      (fun (node: Action_node.t) ->
        let is_package_export =
          List.exists
            (
              function
              | Action.CreateLibrary _
              | Action.CreateExecutable _
              | Action.CreateSharedLibrary _ -> true
              | Action.CompileInterface _
              | Action.CompileImplementation _
              | Action.GenerateInterface _
              | Action.CompileC _
              | Action.CopyFile _
              | Action.WriteFile _
              | Action.BuildForeignDependency _ -> false
            )
            node.value.actions
        in
        if not is_package_export then
          []
        else
          let action_hash_hex = Crypto.Digest.hex (Action_node.get_hash node) in
          List.map
            (fun out_path ->
              Riot_store.Store.{
                name = Path.basename out_path;
                path = out_path;
                action_hash = action_hash_hex
              })
            node.value.outs)
  in
  let seen = HashSet.create () in
  List.filter_map
    (fun (entry: Riot_store.Store.export_entry) ->
      if HashSet.contains seen entry.name then
        None
      else
        (
          let _ = HashSet.insert seen entry.name in
          Some entry
        ))
    entries

let collect_package_artifact_outputs = fun ~sandbox_dir ~outputs ->
  let seen = HashSet.create () in
  outputs |> List.filter_map
    (fun out_path ->
      let abs_path = Path.join sandbox_dir out_path in
      match Path.strip_prefix abs_path ~prefix:sandbox_dir with
      | Ok _ ->
          let abs_path_str = Path.to_string abs_path in
          if HashSet.contains seen abs_path_str then
            None
          else
            (
              let _ = HashSet.insert seen abs_path_str in
              Some abs_path
            )
      | Error _ -> None)

let build = fun ~workspace ~toolchain ~store ~package_graph ~package_key ~(package:Package.t) ~build_ctx ->
  let start = Instant.now () in
  let session_id = build_ctx.Build_ctx.session_id in
  let profile_name = build_ctx.Build_ctx.profile.name in
  let target_triple_str = Kernel.System.Host.to_string (Build_ctx.target_triplet build_ctx) in
  let target_dir =
    Path.(Riot_model.Riot_dirs.out_dir_with_target
      ~workspace_root:workspace.Workspace.root
      ~profile:profile_name
      ~target:target_triple_str
    / Path.v package.Package.name) in
  Log.info ("Package " ^ package.Package.name ^ ": computing content hash with dependencies");
  let package_scope =
    match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
    | Some node -> Riot_planner.Package_graph.get_scope node.value
    | None -> Riot_planner.Package_graph.Runtime
  in
  let emit_visible_progress =
    match package_scope with
    | Riot_planner.Package_graph.Build -> false
    | Riot_planner.Package_graph.Runtime
    | Riot_planner.Package_graph.Dev -> true
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
        (BuildFailed {
          session_id;
          package;
          target = Workspace_planner.Package package.name;
          error = PlanningFailed err
        });
      {
        package_key;
        package;
        status = Failed (PlanningFailed err);
        ocamlc_warnings = [];
        duration;
      }
  | Ok (MissingDependencies { missing; _ }) ->
      let missing_names =
        List.map (fun p -> p.Package.name) missing
      in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let error = "Missing dependencies: " ^ String.concat ", " missing_names in
      (* Don't mark as Failed - this is a transient planning state *)
      let error_variant = ExecutionFailed { message = error } in
      Telemetry.emit
        (BuildFailed {
          session_id;
          package;
          target = Workspace_planner.Package package.name;
          error = error_variant
        });
      {
        package_key;
        package;
        status = Failed error_variant;
        ocamlc_warnings = [];
        duration;
      }
  | Ok (FailedDependencies { failed; _ }) ->
      let failed_names =
        List.map (fun p -> p.Package.name) failed
      in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let reason = "needs " ^ summarize_package_names failed_names in
      Log.info ("Package " ^ package.name ^ ": SKIPPED (" ^ reason ^ ")");
      (* Mark as Skipped in graph so dependents see it as failed *)
      (
        match Riot_planner.Package_graph.get_node_by_key package_graph package_key with
        | Some node -> node.value <- Riot_planner.Package_graph.Skipped {
          package;
          scope = Riot_planner.Package_graph.get_scope node.value;
          reason
        }
        | None -> ()
      );
      Telemetry.emit
        (BuildSkipped {
          session_id;
          package;
          target = Workspace_planner.Package package.name;
          reason
        });
      {
        package_key;
        package;
        status = Skipped { reason };
        ocamlc_warnings = [];
        duration;
      }
  | Ok (Riot_planner.Package_planner.Cached {
    package_key=planned_key;
    hash=package_hash;
    artifact;
    depset;
    exports
  }) ->
      let materialized = Ok () in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      (
        match materialized with
        | Ok () ->
            (
              match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
              | Some node ->
                  node.value <- Riot_planner.Package_graph.Cached {
                    package;
                    scope = Riot_planner.Package_graph.get_scope node.value;
                    hash = package_hash;
                    artifact;
                    depset;
                    exports;
                  }
              | None -> ()
            );
            if emit_visible_progress && List.length artifact.ocamlc_warnings > 0 then
              Telemetry.emit
                (
                  PackageOcamlcWarnings {
                    session_id;
                    package;
                    target = Workspace_planner.Package package.name;
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
                    target = Workspace_planner.Package package.name;
                    status = `Cached;
                    duration;
                  }
                );
            {
              package_key = planned_key;
              package;
              status = Cached artifact;
              ocamlc_warnings = artifact.ocamlc_warnings;
              duration;
            }
        | Error message ->
            let error = ExecutionFailed { message } in
            (
              match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
              | Some node -> node.value <- Riot_planner.Package_graph.Failed {
                package;
                scope = Riot_planner.Package_graph.get_scope node.value;
                hash = package_hash;
                error = message
              }
              | None -> ()
            );
            Telemetry.emit
              (BuildFailed {
                session_id;
                package;
                target = Workspace_planner.Package package.name;
                error
              });
            {
              package_key = planned_key;
              package;
              status = Failed error;
              ocamlc_warnings = [];
              duration;
            }
      )
  | Ok (Planned {
    package_key=planned_key;
    hash=package_hash;
    depset;
    module_graph;
    action_graph
  }) -> (
      Log.info ("Package " ^ package.Package.name ^ ": hash=" ^ Std.Crypto.Digest.hex package_hash);
      if emit_visible_progress then
        Telemetry.emit
          (BuildStarted { session_id; package; target = Workspace_planner.Package package.name });
      Log.info ("Package " ^ package.name ^ ": executing action graph");
      Log.info
        ("Package "
        ^ package.name
        ^ ": executing action graph with "
        ^ Int.to_string (List.length (Action_graph.nodes action_graph))
        ^ " nodes");
      if emit_visible_progress && List.length (Action_graph.nodes action_graph) > 0 then
        Telemetry.emit
          (CompilationStarted {
            session_id;
            package;
            target = Workspace_planner.Package package.name
          });
      (
        match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
        | Some node ->
            node.value <- Riot_planner.Package_graph.Planned {
              package;
              scope = Riot_planner.Package_graph.get_scope node.value;
              module_graph;
              action_graph;
              hash = package_hash;
            }
        | None -> ()
      );
      let inputs = List.concat
        [ package.sources.src; package.sources.native; package.sources.tests; ] in
      let outputs =
        List.concat_map
          (fun (node: Action_node.t) -> node.value.outs)
          (Action_graph.nodes action_graph)
      in
      let do_build sandbox =
        let exec_result = Action_executor.execute
          ~action_graph
          ~sandbox
          ~store
          ~session_id
          toolchain
          ~concurrency:build_ctx.Build_ctx.available_parallelism in
        (* Check if any actions failed *)
        let failed_actions =
          HashMap.to_list exec_result.completed
          |> List.filter_map
            (fun ((_id, result)) ->
              match result.Action_executor.status with
              | Action_executor.Failed err -> Some err
              | _ -> None)
        in
        match failed_actions with
        | first_error :: _ -> Error (convert_action_error first_error)
        | [] ->
            let sandbox_dir = Sandbox.get_dir sandbox in
            let export_entries = compute_export_entries action_graph in
            let package_outputs = collect_package_artifact_outputs ~sandbox_dir ~outputs in
            let ocamlc_warnings = collect_ocamlc_warnings exec_result.completed in
            Ok (sandbox_dir, package_outputs, export_entries, ocamlc_warnings)
      in
      match Sandbox.with_sandbox
        ~workspace
        ~profile:profile_name
        ~target:target_triple_str
        ~package
        ~inputs
        ~depset
        ~store
        ~expected_outputs:outputs
        do_build with
      | exception exn ->
          let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
          let error_msg = "Exception: " ^ Exception.to_string exn in
          let error = ExecutionFailed { message = error_msg } in
          (* Mark as Failed in package graph *)
          (
            match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
            | Some node -> node.value <- Riot_planner.Package_graph.Failed {
              package;
              scope = Riot_planner.Package_graph.get_scope node.value;
              hash = package_hash;
              error = error_msg
            }
            | None -> ()
          );
          Telemetry.emit
            (BuildFailed {
              session_id;
              package;
              target = Workspace_planner.Package package.name;
              error
            });
          {
            package_key = planned_key;
            package;
            status = Failed error;
            ocamlc_warnings = [];
            duration;
          }
      | Ok (sandbox_dir, package_outputs, export_entries, ocamlc_warnings) ->
          Riot_store.Store.materialize_package_exports store ~exports:export_entries ~target_dir
          |> Result.expect ~msg:("Failed to materialize package exports for " ^ package.name);
          let artifact = Riot_store.Store.save
            store
            ~package:package.name
            ~ocamlc_warnings
            ~exports:export_entries
            ~hash:package_hash
            ~sandbox_dir
            ~outs:package_outputs
          |> Result.expect ~msg:("Failed to save package hash artifact for " ^ package.name) in
          if emit_visible_progress && List.length ocamlc_warnings > 0 then
            Telemetry.emit
              (
                PackageOcamlcWarnings {
                  session_id;
                  package;
                  target = Workspace_planner.Package package.name;
                  source = `Fresh;
                  messages = ocamlc_warnings;
                }
              );
          (
            match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
            | Some node ->
                node.value <- Riot_planner.Package_graph.Built {
                  package;
                  scope = Riot_planner.Package_graph.get_scope node.value;
                  module_graph;
                  action_graph;
                  hash = package_hash;
                  artifact;
                  status = Riot_planner.Package_graph.Fresh;
                  depset;
                }
            | None -> ()
          );
          let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
          if emit_visible_progress then
            Telemetry.emit
              (
                BuildCompleted {
                  session_id;
                  package;
                  target = Workspace_planner.Package package.name;
                  status = `Fresh;
                  duration;
                }
              );
          {
            package_key = planned_key;
            package;
            status = Built artifact;
            ocamlc_warnings;
            duration;
          }
      | Error err ->
          let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
          let error_str = package_error_to_string err in
          (* Mark as Failed in package graph *)
          (
            match Riot_planner.Package_graph.get_node_by_key package_graph planned_key with
            | Some node -> node.value <- Riot_planner.Package_graph.Failed {
              package;
              scope = Riot_planner.Package_graph.get_scope node.value;
              hash = package_hash;
              error = error_str
            }
            | None -> ()
          );
          Telemetry.emit
            (BuildFailed {
              session_id;
              package;
              target = Workspace_planner.Package package.name;
              error = err
            });
          {
            package_key = planned_key;
            package;
            status = Failed err;
            ocamlc_warnings = [];
            duration;
          }
    )
