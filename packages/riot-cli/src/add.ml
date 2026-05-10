open Std
open Std.Collections
open Std.Result.Syntax
open Riot_model

type workspace_bootstrap_error =
  | BootstrapManifestWriteFailed of {
      path: Path.t;
      error: IO.error;
    }
  | BootstrapDependencyHashFailed of Riot_deps.Lock_refresh.error
  | BootstrapLockfileWriteFailed of Riot_deps.Lockfile_store.error

type workspace_load_error =
  | WorkspaceScanFailed of Riot_model.Workspace_manager.scan_error
  | WorkspaceLoadHadErrors of Riot_model.Workspace_manager.load_error list

type error =
  | MissingDependency
  | ConflictingTarget
  | ConflictingScope
  | InvalidPackageName of Riot_model.Package_name.error
  | CurrentDirUnavailable of Path.error
  | WorkspaceBootstrapFailed of workspace_bootstrap_error
  | WorkspaceLoadFailed of workspace_load_error
  | AddFailed of Riot_deps.package_error

let out = eprintln

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "add"
  |> about "Add a registry, local path, or GitHub dependency and refresh riot.lock"
  |> args
    [
      positional "dependency"
      |> multiple
      |> help
        "Dependency spec: <name>, <name>@<version>, ../path, github.com/<owner>/<repo>[/pkg][#ref], or https://github.com/<owner>/<repo>[/pkg][#ref]";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Edit a specific workspace package manifest";
      flag "workspace"
      |> long "workspace"
      |> help "Edit the workspace root manifest";
      flag "build"
      |> long "build"
      |> help "Write into [build-dependencies]";
      flag "dev"
      |> long "dev"
      |> help "Write into [dev-dependencies]";
      flag "json"
      |> long "json"
      |> help "Render events as JSON";
    ]

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

let workspace_bootstrap_error_message = fun __tmp1 ->
  match __tmp1 with
  | BootstrapManifestWriteFailed { path; error } ->
      "failed to write manifest '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | BootstrapDependencyHashFailed error -> Riot_deps.Lock_refresh.error_message error
  | BootstrapLockfileWriteFailed error -> Riot_deps.Lockfile_store.error_message error

let workspace_load_error_message = fun __tmp1 ->
  match __tmp1 with
  | WorkspaceScanFailed error -> Riot_model.Workspace_manager.scan_error_message error
  | WorkspaceLoadHadErrors errors ->
      errors
      |> List.map ~fn:Riot_model.Workspace_manager.load_error_to_string
      |> String.concat "; "

let message = fun __tmp1 ->
  match __tmp1 with
  | MissingDependency -> "missing dependency name"
  | ConflictingTarget -> "cannot combine --workspace with --package"
  | ConflictingScope -> "cannot combine --build with --dev"
  | InvalidPackageName error -> Package_name.error_message error
  | CurrentDirUnavailable error ->
      "failed to determine current directory: " ^ path_error_message error
  | WorkspaceBootstrapFailed error ->
      "failed to initialize riot workspace: " ^ workspace_bootstrap_error_message error
  | WorkspaceLoadFailed error ->
      "failed to load initialized riot workspace: " ^ workspace_load_error_message error
  | AddFailed error -> Package_error.message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let selection_of_matches = fun ?(default_selection = Riot_deps.Current) matches ->
  let package = ArgParser.get_one matches "package" in
  let workspace = ArgParser.get_flag matches "workspace" in
  match (package, workspace) with
  | (Some _, true) -> Error ConflictingTarget
  | (Some package, false) ->
      let* package_name =
        Package_name.from_string package
        |> Result.map_err ~fn:(fun error -> InvalidPackageName error)
      in
      Ok (Riot_deps.Package package_name)
  | (None, true) -> Ok Riot_deps.Workspace
  | (None, false) -> Ok default_selection

let scope_of_matches = fun matches ->
  let build = ArgParser.get_flag matches "build" in
  let dev = ArgParser.get_flag matches "dev" in
  match (build, dev) with
  | (true, true) -> Error ConflictingScope
  | (true, false) -> Ok Riot_deps.Build
  | (false, true) -> Ok Riot_deps.Dev
  | (false, false) -> Ok Riot_deps.Runtime

let write_event = fun ~ui ~pm_session_id kind ->
  Riot_model.Event.create
    ~session_id:pm_session_id
    ~level:Riot_model.Event.Info
    (Riot_model.Event.Deps kind)
  |> fun event -> Ui.send ui event

let empty_workspace_manifest_source = {|[workspace]
members = []

[dependencies]
|}

let bootstrap_empty_workspace = fun ~root ->
  let manifest_path = Path.(root / Path.v "riot.toml") in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* () =
    Fs.write empty_workspace_manifest_source manifest_path
    |> Result.map_err
      ~fn:(fun error ->
        WorkspaceBootstrapFailed (BootstrapManifestWriteFailed { path = manifest_path; error }))
  in
  let* dependency_hash =
    Riot_deps.Lock_refresh.dependency_hash
      ~workspace_manager
      ~workspace_root:root
      ~manifest_paths:[ manifest_path ]
    |> Result.map_err
      ~fn:(fun error -> WorkspaceBootstrapFailed (BootstrapDependencyHashFailed error))
  in
  let lockfile = Riot_model.Lockfile.{ format_version = 1; dependency_hash; packages = [] } in
  Riot_deps.Lockfile_store.write ~workspace_root:root lockfile
  |> Result.map_err ~fn:(fun error -> WorkspaceBootstrapFailed (BootstrapLockfileWriteFailed error))

let load_workspace = fun ~root ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* (workspace, load_errors) =
    Riot_model.Workspace_manager.scan workspace_manager root
    |> Result.map_err ~fn:(fun error -> WorkspaceLoadFailed (WorkspaceScanFailed error))
  in
  if List.is_empty load_errors then
    Ok workspace
  else
    Error (WorkspaceLoadFailed (WorkspaceLoadHadErrors load_errors))

let run_request = fun ?(default_selection = Riot_deps.Current) ~workspace ~cwd matches ->
  let mode = Ui.mode_of_json_flag (ArgParser.get_flag matches "json") in
  let ui = Ui.make ~mode () in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let dependencies =
    match ArgParser.get_many matches "dependency" with
    | [] -> Error MissingDependency
    | dependencies -> Ok dependencies
  in
  match (dependencies, selection_of_matches ~default_selection matches, scope_of_matches matches) with
  | (Ok dependencies, Ok selection, Ok scope) ->
      let request: Riot_deps.add_request = Riot_deps.{ selection; scope; dependencies } in
      let pm_session_id = Riot_model.Session_id.make () in
      (
        match Riot_deps.add
          ~on_event:(write_event ~ui ~pm_session_id)
          ~workspace_manager
          ~workspace
          ~cwd
          ~request
          () with
        | Ok () -> Ok ()
        | Error error -> fail (AddFailed error)
      )
  | (Error err, _, _)
  | (_, Error err, _)
  | (_, _, Error err) -> fail err

let run = fun ~workspace matches ->
  match Env.current_dir () with
  | Ok cwd -> run_request ~workspace ~cwd matches
  | Error err -> fail (CurrentDirUnavailable err)

let run_without_workspace = fun ~cwd matches ->
  match bootstrap_empty_workspace ~root:cwd with
  | Error err -> fail err
  | Ok () -> (
      match load_workspace ~root:cwd with
      | Error err -> fail err
      | Ok workspace -> run_request ~default_selection:Riot_deps.Workspace ~workspace ~cwd matches
    )
