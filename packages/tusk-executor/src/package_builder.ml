open Std
open Std.Collections
open Std.Time
open Tusk_model
open Tusk_planner
open Telemetry_events

type package_error = Telemetry_events.package_error =
  | PlanningFailed of Tusk_planner.Planning_error.t
  | ExecutionFailed of { message : string }
  | ActionExecutionFailed of { message : string }
  | ActionOutputsNotCreated of { missing : Path.t list }
  | ActionDependenciesFailed of { failed : Graph.SimpleGraph.Node_id.t list }

let convert_action_error = function
  | Action_executor.ExecutionFailed { message } ->
      Telemetry_events.ActionExecutionFailed { message }
  | Action_executor.OutputsNotCreated { missing } ->
      Telemetry_events.ActionOutputsNotCreated { missing }
  | Action_executor.DependenciesFailed { failed } ->
      Telemetry_events.ActionDependenciesFailed { failed }

let package_error_to_string = function
  | PlanningFailed err ->
      format "Planning failed: %s" (Planning_error.to_string err)
  | ExecutionFailed { message } -> format "Execution failed: %s" message
  | ActionExecutionFailed { message } -> format "Action failed: %s" message
  | ActionOutputsNotCreated { missing } ->
      format "Outputs not created: %s"
        (String.concat ", " (List.map Path.to_string missing))
  | ActionDependenciesFailed { failed } ->
      format "Dependencies failed: %d actions" (List.length failed)

let package_error_to_json = function
  | PlanningFailed planning_err ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "planning_failed");
          ("error", Tusk_planner.Planning_error.to_json planning_err);
        ]
  | ExecutionFailed { message } ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "execution_failed");
          ("message", Std.Data.Json.String message);
        ]
  | ActionExecutionFailed { message } ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "action_failed");
          ("message", Std.Data.Json.String message);
        ]
  | ActionOutputsNotCreated { missing } ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "outputs_not_created");
          ( "missing",
            Std.Data.Json.Array
              (List.map (fun p -> Std.Data.Json.String (Path.to_string p)) missing)
          );
        ]
  | ActionDependenciesFailed { failed } ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "dependencies_failed");
          ( "failed_count",
            Std.Data.Json.String (Int.to_string (List.length failed)) );
        ]

type build_status =
  | Cached of Tusk_store.Artifact.t
  | Built of Tusk_store.Artifact.t
  | Failed of package_error

let build_status_to_json = function
  | Cached artifact ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "cached");
          ("artifact", Tusk_store.Artifact.to_json artifact);
        ]
  | Built artifact ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "built");
          ("artifact", Tusk_store.Artifact.to_json artifact);
        ]
  | Failed err ->
      Std.Data.Json.Object
        [
          ("type", Std.Data.Json.String "failed");
          ("error", package_error_to_json err);
        ]

type build_result = {
  package : Package.t;
  status : build_status;
  duration : Duration.t;
}

let build_result_to_json result =
  Std.Data.Json.Object
    [
      ("package", Package.to_json result.package);
      ("status", build_status_to_json result.status);
      ( "duration_ms",
        Std.Data.Json.Int
          (int_of_float (Duration.to_secs_float result.duration *. 1000.0)) );
    ]

let collect_source_files package =
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
              if String.starts_with ~prefix:"/" path_str then file_path
              else Path.(src_dir / file_path)
            in
            Some abs_path
          else None)
        all_files

let build ~workspace ~toolchain ~store ~package_graph ~package =
  let start = Instant.now () in
  let target_dir =
    Path.(
      workspace.Workspace.root / Path.v "target" / Path.v "debug" / Path.v "out"
      / Path.v package.Package.name)
  in

  Log.info "Package %s: computing content hash with dependencies"
    package.Package.name;
  match
    Tusk_planner.plan_package_with_graph ~workspace ~toolchain ~store
      ~package_graph ~package
  with
  | Error err ->
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      (* Don't mark as Failed in graph - planning errors don't have a hash *)
      Telemetry.emit
        (BuildFailed
           {
             package;
             target = Workspace_planner.Package package.name;
             error = PlanningFailed err;
           });
      { package; status = Failed (PlanningFailed err); duration }
  | Ok (MissingDependencies { missing; _ }) ->
      let missing_names = List.map (fun p -> p.Package.name) missing in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let error =
        format "Missing dependencies: %s" (String.concat ", " missing_names)
      in
      (* Don't mark as Failed - this is a transient planning state *)
      let error_variant = ExecutionFailed { message = error } in
      Telemetry.emit
        (BuildFailed
           {
             package;
             target = Workspace_planner.Package package.name;
             error = error_variant;
           });
      { package; status = Failed error_variant; duration }
  | Ok (FailedDependencies { failed; _ }) ->
      let failed_names = List.map (fun p -> p.Package.name) failed in
      let duration = Instant.duration_since ~earlier:start (Instant.now ()) in
      let reason = format "needs %s" (String.concat ", " failed_names) in
      Log.info "Package %s: SKIPPED (%s)" package.name reason;

      (* Mark as Skipped in graph so dependents see it as failed *)
      (match Tusk_planner.Package_graph.get_node package_graph package with
      | Some node ->
          node.value <- Tusk_planner.Package_graph.Skipped { package; reason }
      | None -> ());

      Telemetry.emit
        (BuildSkipped
           { package; target = Workspace_planner.Package package.name; reason });

      {
        package;
        status =
          Failed (ExecutionFailed { message = format "Skipped (%s)" reason });
        duration;
      }
  | Ok (Planned { module_graph; action_graph; hash = package_hash; depset; _ })
    -> (
      Log.info "Package %s: hash=%s" package.Package.name
        (Std.Crypto.Digest.hex package_hash);

      Telemetry.emit
        (BuildStarted
           { package; target = Workspace_planner.Package package.name });

      match Tusk_store.Store.get store package_hash with
      | Some artifact ->
          Log.info "Package %s: CACHE HIT - skipping execution" package.name;

          let _ =
            Tusk_store.Store.promote store package_hash ~target_dir
            |> Result.expect
                 ~msg:
                   (format "Failed to promote cached artifacts for %s"
                      package.name)
          in

          (* Mark as Built with Cached status *)
          (match Tusk_planner.Package_graph.get_node package_graph package with
          | Some node ->
              node.value <-
                Tusk_planner.Package_graph.Built
                  {
                    package;
                    module_graph;
                    action_graph;
                    hash = package_hash;
                    artifact;
                    status = Tusk_planner.Package_graph.Cached;
                    depset;
                  }
          | None -> ());

          let duration =
            Instant.duration_since ~earlier:start (Instant.now ())
          in
          Telemetry.emit
            (BuildCompleted
               {
                 package;
                 target = Workspace_planner.Package package.name;
                 status = `Cached;
                 duration;
               });
          { package; status = Cached artifact; duration }
      | None -> (
          Log.info "Package %s: CACHE MISS - executing action graph"
            package.name;
          Log.info "Package %s: executing action graph with %d nodes"
            package.name
            (List.length (Action_graph.nodes action_graph));

          (* Mark as Planned in package graph *)
          (match Tusk_planner.Package_graph.get_node package_graph package with
          | Some node ->
              node.value <-
                Tusk_planner.Package_graph.Planned
                  { package; module_graph; action_graph; hash = package_hash }
          | None -> ());

          let inputs =
            List.concat
              [
                package.sources.src;
                package.sources.native;
                package.sources.tests;
              ]
          in
          let outputs =
            List.concat_map
              (fun (node : Action_node.t) -> node.value.outs)
              (Action_graph.nodes action_graph)
          in

          let do_build sandbox =
            let sandbox_dir = Sandbox.get_dir sandbox in
            let exec_result =
              Parallel_action_executor.execute ~action_graph ~sandbox ~store
                toolchain ~concurrency:System.available_parallelism
            in

            (* Check if any actions failed *)
            let failed_actions =
              HashMap.to_list exec_result.completed
              |> List.filter_map (fun (_id, result) ->
                  match result.Action_executor.status with
                  | Action_executor.Failed err -> Some err
                  | _ -> None)
            in

            match failed_actions with
            | first_error :: _ -> Error (convert_action_error first_error)
            | [] -> (
                (* All actions succeeded, save the artifacts *)
                match
                  Tusk_store.Store.save store ~package:package.name
                    ~hash:package_hash ~sandbox_dir ~outs:outputs
                with
                | Ok artifact -> Ok artifact
                | Error msg ->
                    Error
                      (ExecutionFailed
                         {
                           message =
                             format "Failed to save artifacts for %s: %s"
                               package.name msg;
                         }))
          in

          match
            Sandbox.with_sandbox ~workspace ~package ~inputs ~depset ~store
              ~expected_outputs:outputs do_build
          with
          | exception exn ->
              let duration =
                Instant.duration_since ~earlier:start (Instant.now ())
              in
              let error_msg = format "Exception: %s" (Printexc.to_string exn) in
              let error = ExecutionFailed { message = error_msg } in
              (* Mark as Failed in package graph *)
              (match
                 Tusk_planner.Package_graph.get_node package_graph package
               with
              | Some node ->
                  node.value <-
                    Tusk_planner.Package_graph.Failed
                      { package; hash = package_hash; error = error_msg }
              | None -> ());
              Telemetry.emit
                (BuildFailed
                   {
                     package;
                     target = Workspace_planner.Package package.name;
                     error;
                   });
              { package; status = Failed error; duration }
          | Ok artifact ->
              Tusk_store.Store.promote store package_hash ~target_dir
              |> Result.expect
                   ~msg:
                     (format "Failed to promote artifacts for %s" package.name);

              (* Mark as Built with Fresh status *)
              (match
                 Tusk_planner.Package_graph.get_node package_graph package
               with
              | Some node ->
                  node.value <-
                    Tusk_planner.Package_graph.Built
                      {
                        package;
                        module_graph;
                        action_graph;
                        hash = package_hash;
                        artifact;
                        status = Tusk_planner.Package_graph.Fresh;
                        depset;
                      }
              | None -> ());

              let duration =
                Instant.duration_since ~earlier:start (Instant.now ())
              in
              Telemetry.emit
                (BuildCompleted
                   {
                     package;
                     target = Workspace_planner.Package package.name;
                     status = `Fresh;
                     duration;
                   });
              { package; status = Built artifact; duration }
          | Error err ->
              let duration =
                Instant.duration_since ~earlier:start (Instant.now ())
              in
              let error_str = package_error_to_string err in
              (* Mark as Failed in package graph *)
              (match
                 Tusk_planner.Package_graph.get_node package_graph package
               with
              | Some node ->
                  node.value <-
                    Tusk_planner.Package_graph.Failed
                      { package; hash = package_hash; error = error_str }
              | None -> ());
              Telemetry.emit
                (BuildFailed
                   {
                     package;
                     target = Workspace_planner.Package package.name;
                     error = err;
                   });
              { package; status = Failed err; duration }))
