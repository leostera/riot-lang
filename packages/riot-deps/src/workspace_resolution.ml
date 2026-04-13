open Std

type event_sink = Riot_model.Event.kind -> unit

let no_emit: event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ()) |> Time.Duration.to_millis

let resolved_edge_count = fun (lockfile: Riot_model.Lockfile.t) ->
  List.fold_left
    lockfile.packages
    ~acc:0
    ~fn:(fun total (pkg: Riot_model.Lockfile.package) ->
      total + List.length pkg.dependencies + List.length pkg.build_dependencies + List.length pkg.dev_dependencies)

let manifest_path_for_package = fun (pkg: Riot_model.Package_manifest.t) ->
  Path.(pkg.path / Path.v "riot.toml")

let workspace_manifest_paths = fun (workspace: Riot_model.Workspace.t) ->
  Path.(workspace.root / Path.v "riot.toml") :: List.map workspace.packages ~fn:manifest_path_for_package

let root_packages_for_workspace = fun (workspace: Riot_model.Workspace.t) ->
  List.filter workspace.packages ~fn:Riot_model.Package_manifest.is_workspace_member

let lock_package_version_map = fun (lockfile_opt: Riot_model.Lockfile.t option) ->
  match lockfile_opt with
  | None -> []
  | Some (lockfile: Riot_model.Lockfile.t) ->
      List.fold_left
        lockfile.packages
        ~acc:[]
        ~fn:(fun acc (pkg: Riot_model.Lockfile.package) ->
          match pkg.id.registry, pkg.id.version with
          | Some registry, Some version ->
              (registry ^ ":" ^ pkg.id.name, version) :: acc
          | None, Some version -> (
              match pkg.provenance with
              | Riot_model.Lockfile.Source _ -> ("source:" ^ pkg.id.name, version) :: acc
              | _ -> acc
            )
          | _ ->
              acc)

let emit_locked_packages = fun ~(emit:event_sink) ~(previous_lock:Riot_model.Lockfile.t option) (
  current_lock: Riot_model.Lockfile.t
) ->
  let previous_versions = lock_package_version_map previous_lock in
  List.for_each
    current_lock.packages
    ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
      match pkg.id.registry, pkg.id.version with
      | Some registry, Some version ->
          if Option.is_none
            (List.find
              previous_versions
              ~fn:(fun (key, _) -> String.equal key (registry ^ ":" ^ pkg.id.name)))
          then
            emit (Riot_model.Event.PackageVersionLocked { package = pkg.id.name; version })
      | None, Some version -> (
          match pkg.provenance with
          | Riot_model.Lockfile.Source _ ->
              if Option.is_none
                (List.find
                  previous_versions
                  ~fn:(fun (key, _) -> String.equal key ("source:" ^ pkg.id.name)))
              then
                emit (Riot_model.Event.PackageVersionLocked { package = pkg.id.name; version })
          | _ -> ()
        )
      | _ ->
          ())

let lockfile_with_dependency_hash = fun dependency_hash (lockfile: Riot_model.Lockfile.t) ->
  { lockfile with dependency_hash = dependency_hash }

let ensure_lock = fun ?(emit = no_emit) ?workspace_manager ~mode ~registry ~(workspace:Riot_model.Workspace.t) () ->
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
      let current_dependency_hash =
        match Lock_refresh.dependency_hash ~workspace_manager ~workspace_root ~manifest_paths with
        | Ok dependency_hash -> Ok dependency_hash
        | Error err -> Error (Error.LockRefreshCheckFailed { workspace_root; error = err })
      in
      let lock_result =
        match current_dependency_hash with
        | Error _ as err -> err
        | Ok current_dependency_hash ->
            let solve_started = Time.Instant.now () in
            let lock_result =
              match mode with
              | Dep_solver.Unlock ->
	                  emit
	                    (Riot_model.Event.DependencyResolutionStarted {
	                      packages = List.map packages ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name);
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
	                  |> Result.map ~fn:(lockfile_with_dependency_hash current_dependency_hash)
	                  |> Result.map ~fn:(fun lockfile -> (lockfile, false, true, solve_started))
              | Dep_solver.Refresh -> (
                  let needs_refresh =
                    match existing_lock with
                    | None -> true
                    | Some (lockfile: Riot_model.Lockfile.t) -> not
                      (String.equal lockfile.dependency_hash current_dependency_hash)
                  in
                  if not needs_refresh then
                    (
                      match existing_lock with
                      | Some lockfile -> Ok (lockfile, true, false, solve_started)
                      | None -> Error (Error.LockRefreshCheckFailed {
                        workspace_root;
                        error = "missing existing lockfile during refresh reuse"
                      })
                    )
                  else (
	                    emit
	                      (Riot_model.Event.DependencyResolutionStarted {
	                        packages = List.map packages ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name);
	                        mode = `Refresh
	                      });
                    emit
                      (Riot_model.Event.DependencyResolutionRefreshingLock { path = lock_path_str });
	                    Dep_solver.lock_deps ~emit ~mode ~registry ~existing_lock ~workspace ()
	                    |> Result.map ~fn:(lockfile_with_dependency_hash current_dependency_hash)
	                    |> Result.map ~fn:(fun lockfile -> (lockfile, false, true, solve_started))
                  )
                )
            in
            lock_result
      in
      match lock_result with
      | Error err ->
          emit (Riot_model.Event.DependencyResolutionFailed { error = err });
          Error err
      | Ok (lockfile, used_existing_lock, should_write, solve_started) ->
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
              if should_write then
                emit_locked_packages ~emit ~previous_lock:existing_lock lockfile;
              let projection_emit =
                if used_existing_lock then
                  no_emit
                else
                  emit
              in
              match Projection.resolve_packages
                ~emit:projection_emit
                ~materialize_emit:emit
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

let ensure_workspace = fun ?(emit = no_emit) ?workspace_manager ~mode ~registry ~(workspace:Riot_model.Workspace.t) () ->
  match ensure_lock ~emit ?workspace_manager ~mode ~registry ~workspace () with
  | Error _ as err -> err
  | Ok (_lockfile, resolved_packages) -> Ok {
    workspace
    with packages = List.map resolved_packages ~fn:(fun (pkg: Riot_model.Package.resolved) ->
      Riot_model.Package_manifest.of_package pkg.package)
  }
