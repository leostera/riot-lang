open Std
open Std.Result.Syntax

type event_sink = Riot_model.Event.kind -> unit

let no_emit: event_sink = fun _ -> ()

let duration_ms_since = fun started ->
  Time.Instant.duration_since ~earlier:started (Time.Instant.now ())
  |> Time.Duration.to_millis

let resolved_edge_count = fun (lockfile: Riot_model.Lockfile.t) ->
  List.fold_left
    lockfile.packages
    ~init:0
    ~fn:(fun total (pkg: Riot_model.Lockfile.package) ->
      total
      + List.length pkg.dependencies
      + List.length pkg.build_dependencies
      + List.length pkg.dev_dependencies)

let manifest_path_for_package = fun (pkg: Riot_model.Package_manifest.t) ->
  Path.(pkg.path / Path.v "riot.toml")

let workspace_manifest_paths = fun (workspace: Riot_model.Workspace_manifest.t) ->
  Path.(workspace.root / Path.v "riot.toml")
  :: List.map workspace.packages ~fn:manifest_path_for_package

let root_packages_for_workspace = fun (workspace: Riot_model.Workspace_manifest.t) ->
  List.filter
    workspace.packages
    ~fn:Riot_model.Package_manifest.is_workspace_member

let event_package_names = fun packages ->
  List.map
    packages
    ~fn:(fun (pkg: Riot_model.Package_manifest.t) -> pkg.name)

type lock_package_key =
  | Registry of {
      registry: string;
      package: Riot_model.Package_name.t;
    }
  | Source of Riot_model.Package_name.t

let lock_package_key = fun (pkg: Riot_model.Lockfile.package) ->
  match (pkg.id.registry, pkg.id.version) with
  | (Some registry, Some _) -> Some (Registry { registry; package = pkg.id.name })
  | (None, Some _) -> (
      match pkg.provenance with
      | Riot_model.Lockfile.Source _ -> Some (Source pkg.id.name)
      | _ -> None
    )
  | _ -> None

let lock_package_key_equal = fun left right ->
  match (left, right) with
  | (Registry left, Registry right) ->
      String.equal left.registry right.registry
      && Riot_model.Package_name.equal left.package right.package
  | (Source left, Source right) -> Riot_model.Package_name.equal left right
  | _ -> false

let lock_package_version_map = fun (lockfile_opt: Riot_model.Lockfile.t option) ->
  match lockfile_opt with
  | None -> []
  | Some (lockfile: Riot_model.Lockfile.t) ->
      List.fold_left
        lockfile.packages
        ~init:[]
        ~fn:(fun acc (pkg: Riot_model.Lockfile.package) ->
          match (lock_package_key pkg, pkg.id.version) with
          | (Some key, Some version) -> (key, version) :: acc
          | _ -> acc)

let emit_locked_packages = fun
  ~(emit:event_sink)
  ~(previous_lock:Riot_model.Lockfile.t option)
  (current_lock: Riot_model.Lockfile.t) ->
  let previous_versions = lock_package_version_map previous_lock in
  List.for_each
    current_lock.packages
    ~fn:(fun (pkg: Riot_model.Lockfile.package) ->
      let event_package_name = pkg.id.name in
      match (lock_package_key pkg, pkg.id.version) with
      | (Some key, Some version) ->
          if
            Option.is_none
              (List.find
                previous_versions
                ~fn:(fun (existing_key, _) -> lock_package_key_equal existing_key key))
          then
            emit (Riot_model.Event.PackageVersionLocked { package = event_package_name; version })
      | _ -> ())

let lockfile_with_dependency_hash = fun dependency_hash (lockfile: Riot_model.Lockfile.t) -> {
  lockfile with
  dependency_hash = dependency_hash;
}

let ensure_lock = fun
  ?(emit = no_emit)
  ?existing_lock
  ~workspace_manager
  ~mode
  ~registry
  ~(workspace:Riot_model.Workspace_manifest.t)
  () ->
  let workspace_root = workspace.root in
  let manifest_paths = workspace_manifest_paths workspace in
  let packages = root_packages_for_workspace workspace in
  let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
  let lock_path_str = Path.to_string lock_path in
  let write_lockfile (lockfile: Riot_model.Lockfile.t) =
    emit (Riot_model.Event.LockfileWriteStarted { path = lock_path_str });
    let write_started = Time.Instant.now () in
    match Lockfile_store.write ~workspace_root lockfile with
    | Ok () ->
        emit
          (Riot_model.Event.LockfileWriteFinished {
            path = lock_path_str;
            duration_ms = duration_ms_since write_started;
          });
        Ok ()
    | Error err ->
        let err = Error.LockfileWriteFailed {
          path = lock_path;
          error = Lockfile_store.error_message err;
        }
        in
        emit (Riot_model.Event.LockfileWriteFailed { path = lock_path_str; error = err });
        Error err
  in
  let resolve_lockfile ~projection_emit (lockfile: Riot_model.Lockfile.t) =
    Projection.resolve_packages
      ~emit:projection_emit
      ~materialize_emit:emit
      ~registry
      ~workspace_root
      ~packages
      ~lockfile
      ()
  in
  match Lockfile_store.read ~workspace_root with
  | Error err ->
      let err = Error.LockfileReadFailed {
        path = lock_path;
        error = Lockfile_store.error_message err;
      }
      in
      emit (Riot_model.Event.LockfileReadFailed { path = lock_path_str; error = err });
      Error err
  | Ok stored_lock ->
      let existing_lock =
        match existing_lock with
        | Some override -> override
        | None -> stored_lock
      in
      let current_dependency_hash =
        match Lock_refresh.dependency_hash ~workspace_manager ~workspace_root ~manifest_paths with
        | Ok dependency_hash -> Ok dependency_hash
        | Error err ->
            Error (Error.LockRefreshCheckFailed {
              workspace_root;
              error = Lock_refresh.error_message err;
            })
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
                    (
                      Riot_model.Event.DependencyResolutionStarted {
                        packages = event_package_names packages;
                        mode = `Unlock;
                      }
                    );
                  emit
                    (
                      Riot_model.Event.DependencyResolutionUnlocking {
                        path =
                          match existing_lock with
                          | Some _ -> Some lock_path_str
                          | None ->
                              None;
                      }
                    );
                  Dep_solver.lock_deps ~emit ~mode ~registry ~existing_lock ~workspace ()
                  |> Result.map ~fn:(lockfile_with_dependency_hash current_dependency_hash)
                  |> Result.map ~fn:(fun lockfile -> (lockfile, false, true, solve_started))
              | Dep_solver.Refresh -> (
                  let needs_refresh =
                    match existing_lock with
                    | None -> true
                    | Some (lockfile: Riot_model.Lockfile.t) ->
                        not (String.equal lockfile.dependency_hash current_dependency_hash)
                  in
                  if not needs_refresh then (
                    match existing_lock with
                    | Some lockfile -> Ok (lockfile, true, false, solve_started)
                    | None ->
                        Error (Error.LockRefreshCheckFailed {
                          workspace_root;
                          error = "missing existing lockfile during refresh reuse";
                        })
                  ) else (
                    emit
                      (
                        Riot_model.Event.DependencyResolutionStarted {
                          packages = event_package_names packages;
                          mode = `Refresh;
                        }
                      );
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
              write_lockfile lockfile
            else
              Ok ()
          in
          match write_result with
          | Error err ->
              emit (Riot_model.Event.DependencyResolutionFailed { error = err });
              Error err
          | Ok () -> (
              if used_existing_lock then
                emit
                  (Riot_model.Event.DependencyResolutionUsingExistingLock { path = lock_path_str });
              if should_write then
                emit_locked_packages ~emit ~previous_lock:existing_lock lockfile;
              let projection_emit =
                if used_existing_lock then
                  no_emit
                else
                  emit
              in
              match resolve_lockfile ~projection_emit lockfile with
              | Error _ when used_existing_lock -> (
                  match Lock_refresh.dependency_hash
                    ~workspace_manager
                    ~workspace_root
                    ~manifest_paths with
                  | Error err ->
                      let err = Error.LockRefreshCheckFailed {
                        workspace_root;
                        error = Lock_refresh.error_message err;
                      }
                      in
                      emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                      Error err
                  | Ok dependency_hash ->
                      let solve_started = Time.Instant.now () in
                      emit
                        (
                          Riot_model.Event.DependencyResolutionStarted {
                            packages = event_package_names packages;
                            mode = `Refresh;
                          }
                        );
                      emit
                        (Riot_model.Event.DependencyResolutionRefreshingLock {
                          path = lock_path_str;
                        });
                      match Dep_solver.lock_deps
                        ~emit
                        ~mode:Dep_solver.Refresh
                        ~registry
                        ~existing_lock
                        ~workspace
                        ()
                      |> Result.map ~fn:(lockfile_with_dependency_hash dependency_hash) with
                      | Error err ->
                          emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                          Error err
                      | Ok refreshed_lock -> (
                          match write_lockfile refreshed_lock with
                          | Error err ->
                              emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                              Error err
                          | Ok () ->
                              emit_locked_packages ~emit ~previous_lock:existing_lock refreshed_lock;
                              match resolve_lockfile ~projection_emit:emit refreshed_lock with
                              | Error err ->
                                  emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                                  Error err
                              | Ok resolved ->
                                  emit
                                    (Riot_model.Event.DependencyResolutionFinished {
                                      duration_ms = duration_ms_since solve_started;
                                      resolved_packages = List.length resolved;
                                      resolved_edges = resolved_edge_count refreshed_lock;
                                    });
                                  Ok (refreshed_lock, resolved)
                        )
                )
              | Error err ->
                  emit (Riot_model.Event.DependencyResolutionFailed { error = err });
                  Error err
              | Ok resolved ->
                  if not used_existing_lock then
                    emit
                      (Riot_model.Event.DependencyResolutionFinished {
                        duration_ms = duration_ms_since solve_started;
                        resolved_packages = List.length resolved;
                        resolved_edges = resolved_edge_count lockfile;
                      });
                  Ok (lockfile, resolved)
            )

let ensure_workspace = fun
  ?(emit = no_emit)
  ~workspace_manager
  ~mode
  ~registry
  ~(workspace:Riot_model.Workspace_manifest.t)
  () ->
  let* (_lockfile, resolved_packages) =
    ensure_lock ~workspace_manager ~emit ~mode ~registry ~workspace ()
  in
  Ok (Riot_model.Workspace.make
    ?name:workspace.name
    ~root:workspace.root
    ~packages:(List.map
      resolved_packages
      ~fn:(fun (pkg: Riot_model.Package.resolved) ->
        Riot_model.Package_manifest.from_package
          pkg.package))
    ~dependencies:workspace.dependencies
    ~dev_dependencies:workspace.dev_dependencies
    ~build_dependencies:workspace.build_dependencies
    ~profile_overrides:workspace.profile_overrides
    ~target_dir:(Path.to_string workspace.target_dir_root)
    ())
