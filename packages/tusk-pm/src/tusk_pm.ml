open Std
module Dep_solver = Dep_solver
module Lockfile_store = Lockfile_store
module Lock_refresh = Lock_refresh
module Projection = Projection

type event_sink = Tusk_model.Event.kind -> unit

let no_emit : event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ())
  |> Time.Duration.to_millis

let resolved_edge_count = fun (lockfile: Tusk_model.Lockfile.t) ->
  List.fold_left
    (fun total (pkg: Tusk_model.Lockfile.package) ->
      total
      + List.length pkg.dependencies
      + List.length pkg.build_dependencies
      + List.length pkg.dev_dependencies)
    0
    lockfile.packages

let ensure_lock = fun ?(emit = no_emit) ~mode ~registry ~registry_cache ~registry_name ~workspace_root ~manifest_paths ~packages () ->
  let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
  let lock_path_str = Path.to_string lock_path in
  emit (Tusk_model.Event.LockfileReadStarted { path = lock_path_str });
  let read_started = Time.Instant.now () in
  match Lockfile_store.read ~workspace_root with
  | Error err ->
      emit (Tusk_model.Event.LockfileReadFailed { path = lock_path_str; error = err });
      Error err
  | Ok existing_lock ->
      emit (Tusk_model.Event.LockfileReadFinished {
        path = lock_path_str;
        duration_ms = duration_ms_since read_started;
      });
      let solve_started = Time.Instant.now () in
      let lock_result =
        match mode with
        | Dep_solver.Unlock ->
            emit (Tusk_model.Event.DependencyResolutionStarted {
              packages = List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) packages;
              mode = `Unlock;
            });
            emit (Tusk_model.Event.DependencyResolutionUnlocking {
              path =
                match existing_lock with
                | Some _ -> Some lock_path_str
                | None -> None;
            });
            Dep_solver.lock_deps
              ~mode
              ~registry
              ~registry_cache
              ~registry_name
              ~existing_lock
              packages
        | Dep_solver.Refresh -> (
            match existing_lock with
            | Some lockfile -> (
                match Lock_refresh.needs_refresh ~workspace_root ~manifest_paths with
                | Error err -> Error err
                | Ok false ->
                    emit (Tusk_model.Event.DependencyResolutionUsingExistingLock {
                      path = lock_path_str;
                    });
                    Ok lockfile
                | Ok true ->
                    emit (Tusk_model.Event.DependencyResolutionStarted {
                      packages = List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) packages;
                      mode = `Refresh;
                    });
                    emit (Tusk_model.Event.DependencyResolutionRefreshingLock { path = lock_path_str });
                    Dep_solver.lock_deps
                      ~mode
                      ~registry
                      ~registry_cache
                      ~registry_name
                      ~existing_lock
                      packages
              )
            | None ->
                emit (Tusk_model.Event.DependencyResolutionStarted {
                  packages = List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) packages;
                  mode = `Refresh;
                });
                emit (Tusk_model.Event.DependencyResolutionRefreshingLock { path = lock_path_str });
                Dep_solver.lock_deps
                  ~mode
                  ~registry
                  ~registry_cache
                  ~registry_name
                  ~existing_lock
                  packages
          )
      in
      match lock_result with
      | Error err ->
          emit (Tusk_model.Event.DependencyResolutionFailed { error = err });
          Error err
      | Ok lockfile ->
          let should_write =
            match mode, existing_lock with
            | Dep_solver.Unlock, _ -> true
            | Dep_solver.Refresh, None -> true
            | Dep_solver.Refresh, Some _ -> (
                match Lock_refresh.needs_refresh ~workspace_root ~manifest_paths with
                | Ok needs_refresh -> needs_refresh
                | Error _ -> true
              )
          in
          let write_result =
            if should_write then (
              emit (Tusk_model.Event.LockfileWriteStarted { path = lock_path_str });
              let write_started = Time.Instant.now () in
              match Lockfile_store.write ~workspace_root lockfile with
              | Ok () ->
                  emit (Tusk_model.Event.LockfileWriteFinished {
                    path = lock_path_str;
                    duration_ms = duration_ms_since write_started;
                  });
                  Ok ()
              | Error err ->
                  emit (Tusk_model.Event.LockfileWriteFailed { path = lock_path_str; error = err });
                  Error err
            ) else
              Ok ()
          in
          match write_result with
          | Error err ->
              emit (Tusk_model.Event.DependencyResolutionFailed { error = err });
              Error err
          | Ok () -> (
              match Projection.resolve_packages ~packages ~lockfile with
              | Error err ->
                  emit (Tusk_model.Event.DependencyResolutionFailed { error = err });
                  Error err
              | Ok resolved ->
                  emit (Tusk_model.Event.DependencyResolutionFinished {
                    duration_ms = duration_ms_since solve_started;
                    resolved_packages = List.length resolved;
                    resolved_edges = resolved_edge_count lockfile;
                  });
                  Ok (lockfile, resolved)
            )
