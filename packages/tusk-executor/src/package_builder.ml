open Std
open Tusk_model
open Tusk_planner

type package_error =
  | PlanningFailed of Planning_error.t
  | ExecutionFailed of { message : string }

let package_error_to_string = function
  | PlanningFailed err ->
      format "Planning failed: %s" (Planning_error.to_string err)
  | ExecutionFailed { message } -> format "Execution failed: %s" message

type build_status =
  | Cached of Tusk_store.Artifact.t
  | Built of Tusk_store.Artifact.t
  | Failed of package_error

type build_result = {
  package : Package.t;
  status : build_status;
  duration : Time.Duration.t;
}

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
  let start = Time.Instant.now () in
  let target_dir =
    Path.(
      workspace.Workspace.root / Path.v "target" / Path.v "debug"
      / Path.v package.Package.name)
  in

  Log.info "Package %s: computing content hash with dependencies"
    package.Package.name;
  match
    Package_planner.plan_package ~workspace ~toolchain ~package_graph ~package
  with
  | Error err ->
      let duration =
        Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
      in
      { package; status = Failed (PlanningFailed err); duration }
  | Ok (MissingDependencies { missing; _ }) ->
      let missing_names = List.map (fun p -> p.Package.name) missing in
      let duration =
        Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
      in
      {
        package;
        status =
          Failed
            (ExecutionFailed
               {
                 message =
                   format "Missing dependencies: %s"
                     (String.concat ", " missing_names);
               });
        duration;
      }
  | Ok (Planned { module_graph; action_graph; hash = package_hash; _ }) -> (
      Log.info "Package %s: hash=%s" package.Package.name
        (Std.Crypto.Digest.hex package_hash);

      Telemetry.emit
        Telemetry_events.(
          BuildStarted { package; target = Workspace_planner.Package package.name });

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
          Package_graph.mark_planned package_graph package ~module_graph
            ~action_graph ~hash:package_hash;
          let duration =
            Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
          in
          Telemetry.emit
            Telemetry_events.(
              BuildCompleted
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

          let inputs = collect_source_files package in
          let outputs =
            List.concat_map
              (fun (node : Action_node.t) -> node.value.outs)
              (Action_graph.nodes action_graph)
          in

          match
            Fun.protect
              (fun () ->
                Sandbox.with_sandbox ~workspace ~inputs
                  ~expected_outputs:outputs (fun sandbox ->
                    let sandbox_dir = Sandbox.get_dir sandbox in
                    Action_executor.execute ~action_graph ~sandbox ~store
                      toolchain ~concurrency:4;

                    Tusk_store.Store.save store ~package:package.name
                      ~hash:package_hash ~sandbox_dir ~outs:outputs
                    |> Result.expect
                         ~msg:
                           (format "Failed to save artifacts for %s"
                              package.name)))
              ~finally:(fun () -> ())
          with
          | exception exn ->
              let duration =
                Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
              in
              let error = format "Exception: %s" (Printexc.to_string exn) in
              Telemetry.emit
                Telemetry_events.(
                  BuildFailed
                    { package; target = Workspace_planner.Package package.name; error });
              {
                package;
                status = Failed (ExecutionFailed { message = error });
                duration;
              }
          | artifact ->
              Tusk_store.Store.promote store package_hash ~target_dir
              |> Result.expect
                   ~msg:
                     (format "Failed to promote artifacts for %s" package.name);

              Package_graph.mark_planned package_graph package ~module_graph
                ~action_graph ~hash:package_hash;

              let duration =
                Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
              in
              Telemetry.emit
                Telemetry_events.(
                  BuildCompleted
                    {
                      package;
                      target = Workspace_planner.Package package.name;
                      status = `Fresh;
                      duration;
                    });
              { package; status = Built artifact; duration }))
