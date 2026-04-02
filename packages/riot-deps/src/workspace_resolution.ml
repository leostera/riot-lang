open Std

type event_sink = Riot_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

let resolved_edge_count = fun (lockfile: Riot_model.Lockfile.t) ->
  List.fold_left
    (fun total (pkg: Riot_model.Lockfile.package) ->
      total + List.length pkg.dependencies + List.length pkg.build_dependencies + List.length pkg.dev_dependencies)
    0
    lockfile.packages

let manifest_path_for_package = fun (pkg: Riot_model.Package.t) ->
  Path.(pkg.path / Path.v "riot.toml")

let workspace_manifest_paths = fun (workspace: Riot_model.Workspace.t) ->
  Path.(workspace.root / Path.v "riot.toml") :: List.map manifest_path_for_package workspace.packages

let root_packages_for_workspace = fun (workspace: Riot_model.Workspace.t) ->
  List.filter Riot_model.Package.is_workspace_member workspace.packages

let ensure_lock = fun ?(emit = no_emit) ~mode ~registry ~(workspace:Riot_model.Workspace.t) () ->
  let workspace_root = workspace.root in
  let manifest_paths = workspace_manifest_paths workspace in
  let packages = root_packages_for_workspace workspace in
  let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
  let lock_path_str = Path.to_string lock_path in
  match Lockfile_store.read ~workspace_root with
  | Error err ->
      let err = Error.LockfileReadFailed { path = lock_path; error = err } in
      emit (Riot_model.Event.LockfileReadFailed { path = lock_path_str; error = err });
      Error err
  | Ok existing_lock ->
      let solve_started = Time.Instant.now () in
      let lock_result =
        match mode with
        | Dep_solver.Unlock ->
            emit
              (Riot_model.Event.DependencyResolutionStarted {
                packages = List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) packages;
                mode = `Unlock
              });
            emit
              (
                Riot_model.Event.DependencyResolutionUnlocking {
                  path =
                    match existing_lock with
                    | Some _ -> Some lock_path_str
                    | None -> None;
                }
              );
            Dep_solver.lock_deps ~emit ~mode ~registry ~existing_lock ~workspace ()
            |> Result.map (fun lockfile -> (lockfile, false))
        | Dep_solver.Refresh -> (
            match existing_lock with
            | Some lockfile -> (
                match Lock_refresh.needs_refresh ~workspace_root ~manifest_paths with
                | Error err ->
                    Error (Error.LockRefreshCheckFailed { workspace_root; error = err })
                | Ok false ->
                    Ok (lockfile, true)
                | Ok true ->
                    emit
                      (Riot_model.Event.DependencyResolutionStarted {
                        packages = List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) packages;
                        mode = `Refresh
                      });
                    emit
                      (Riot_model.Event.DependencyResolutionRefreshingLock { path = lock_path_str });
                    Dep_solver.lock_deps ~emit ~mode ~registry ~existing_lock ~workspace ()
                    |> Result.map (fun lockfile -> (lockfile, false))
              )
            | None ->
                emit
                  (Riot_model.Event.DependencyResolutionStarted {
                    packages = List.map (fun (pkg: Riot_model.Package.t) -> pkg.name) packages;
                    mode = `Refresh
                  });
                emit (Riot_model.Event.DependencyResolutionRefreshingLock { path = lock_path_str });
                Dep_solver.lock_deps ~emit ~mode ~registry ~existing_lock ~workspace ()
                |> Result.map (fun lockfile -> (lockfile, false))
          )
      in
      match lock_result with
      | Error err ->
          emit (Riot_model.Event.DependencyResolutionFailed { error = err });
          Error err
      | Ok (lockfile, used_existing_lock) ->
          let should_write =
            match mode, existing_lock with
            | Dep_solver.Unlock, _ ->
                true
            | Dep_solver.Refresh, None ->
                true
            | Dep_solver.Refresh, Some _ -> (
                match Lock_refresh.needs_refresh ~workspace_root ~manifest_paths with
                | Ok needs_refresh -> needs_refresh
                | Error _ -> true
              )
          in
          let write_result =
            if should_write then
              (
                emit (Riot_model.Event.LockfileWriteStarted { path = lock_path_str });
                let write_started = Time.Instant.now () in
                match Lockfile_store.write ~workspace_root lockfile with
                | Ok () ->
                    emit
                      (Riot_model.Event.LockfileWriteFinished {
                        path = lock_path_str;
                        duration_ms = duration_ms_since write_started
                      });
                    Ok ()
                | Error err ->
                    let err = Error.LockfileWriteFailed { path = lock_path; error = err } in
                    emit (Riot_model.Event.LockfileWriteFailed { path = lock_path_str; error = err });
                    Error err
              )
            else
              Ok ()
          in
          match write_result with
          | Error err ->
              emit (Riot_model.Event.DependencyResolutionFailed { error = err });
              Error err
          | Ok () -> (
              match Materializer.ensure_packages ~emit ~registry ~lockfile () with
              | Error err ->
                  emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                  Error err
              | Ok () -> (
                  let projection_emit =
                    if used_existing_lock then
                      no_emit
                    else
                      emit
                  in
                  match Projection.resolve_packages
                    ~emit:projection_emit
                    ~registry
                    ~workspace_root
                    ~packages
                    ~lockfile
                    () with
                  | Error err ->
                      emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                      Error err
                  | Ok resolved ->
                      if not used_existing_lock then
                        emit
                          (Riot_model.Event.DependencyResolutionFinished {
                            duration_ms = duration_ms_since solve_started;
                            resolved_packages = List.length resolved;
                            resolved_edges = resolved_edge_count lockfile
                          });
                      Ok (lockfile, resolved)
                )
            )

let ensure_workspace = fun ?(emit = no_emit) ~mode ~registry ~(workspace:Riot_model.Workspace.t) () ->
  match ensure_lock ~emit ~mode ~registry ~workspace () with
  | Error _ as err -> err
  | Ok (_lockfile, resolved_packages) -> Ok {
    workspace
    with packages = List.map (fun (pkg: Riot_model.Package.resolved) -> pkg.package) resolved_packages
  }
