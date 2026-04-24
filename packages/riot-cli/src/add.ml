open Std
open Std.Collections
open Std.Result.Syntax
open Riot_model

type error =
  | MissingDependency
  | ConflictingTarget
  | ConflictingScope
  | InvalidPackageName of string
  | CurrentDirUnavailable of string
  | WorkspaceBootstrapFailed of string
  | WorkspaceLoadFailed of string
  | AddFailed of Riot_deps.package_error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "add"
    |> about "Add a registry, local path, or GitHub dependency and refresh riot.lock"
    |> args
      [
        positional "dependency" |> help "Dependency spec: <name>, <name>@<version>, ../path, github.com/<owner>/<repo>[/pkg][#ref], or https://github.com/<owner>/<repo>[/pkg][#ref]";
        option "package" |> short 'p' |> long "package" |> help "Edit a specific workspace package manifest";
        flag "workspace" |> long "workspace" |> help "Edit the workspace root manifest";
        flag "build" |> long "build" |> help "Write into [build-dependencies]";
        flag "dev" |> long "dev" |> help "Write into [dev-dependencies]";
        flag "json" |> long "json" |> help "Render events as JSON";
      ]

let message = function
  | MissingDependency -> "missing dependency name"
  | ConflictingTarget -> "cannot combine --workspace with --package"
  | ConflictingScope -> "cannot combine --build with --dev"
  | InvalidPackageName error -> error
  | CurrentDirUnavailable error -> "failed to determine current directory: " ^ error
  | WorkspaceBootstrapFailed error -> "failed to initialize riot workspace: " ^ error
  | WorkspaceLoadFailed error -> "failed to load initialized riot workspace: " ^ error
  | AddFailed error -> Package_error.message error

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let selection_of_matches = fun ?(default_selection = Riot_deps.Current) matches ->
  let package = ArgParser.get_one matches "package" in
  let workspace = ArgParser.get_flag matches "workspace" in
  match package, workspace with
  | Some _, true ->
      Error ConflictingTarget
  | Some package, false ->
      let* package_name = Package_name.from_string package
      |> Result.map_err ~fn:(fun error -> InvalidPackageName (Package_name.error_message error)) in
      Ok (Riot_deps.Package package_name)
  | None, true ->
      Ok Riot_deps.Workspace
  | None, false ->
      Ok default_selection

let scope_of_matches = fun matches ->
  let build = ArgParser.get_flag matches "build" in
  let dev = ArgParser.get_flag matches "dev" in
  match build, dev with
  | true, true -> Error ConflictingScope
  | true, false -> Ok Riot_deps.Build
  | false, true -> Ok Riot_deps.Dev
  | false, false -> Ok Riot_deps.Runtime

let write_event = fun ~mode ~pm_session_id ~seen_registry_updates kind ->
  Riot_model.Event.create ~session_id:pm_session_id ~level:Riot_model.Event.Info kind
  |> Build.write_pm_event ~mode ~seen_registry_updates

let empty_workspace_manifest_source = {|[workspace]
members = []

[dependencies]
|}

let bootstrap_empty_workspace = fun ~root ->
  let manifest_path = Path.(root / Path.v "riot.toml") in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* () = Fs.write empty_workspace_manifest_source manifest_path
  |> Result.map_err ~fn:(fun err -> WorkspaceBootstrapFailed (IO.error_message err)) in
  let* dependency_hash = Riot_deps.Lock_refresh.dependency_hash
    ~workspace_manager
    ~workspace_root:root
    ~manifest_paths:[ manifest_path ]
  |> Result.map_err
    ~fn:(fun err -> WorkspaceBootstrapFailed (Riot_deps.Lock_refresh.error_message err)) in
  let lockfile = Riot_model.Lockfile.{ format_version = 1; dependency_hash; packages = [] } in
  Riot_deps.Lockfile_store.write ~workspace_root:root lockfile
  |> Result.map_err
    ~fn:(fun err -> WorkspaceBootstrapFailed (Riot_deps.Lockfile_store.error_message err))

let load_workspace = fun ~root ->
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let* (workspace, load_errors) = Riot_model.Workspace_manager.scan workspace_manager root
  |> Result.map_err
    ~fn:(fun err -> WorkspaceLoadFailed (Riot_model.Workspace_manager.scan_error_message err)) in
  if List.is_empty load_errors then
    Ok workspace
  else
    let error = load_errors
    |> List.map ~fn:Riot_model.Workspace_manager.load_error_to_string
    |> String.concat "; " in
    Error (WorkspaceLoadFailed error)

let run_request = fun ?(default_selection = Riot_deps.Current) ~workspace ~cwd matches ->
  let mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let dependency =
    match ArgParser.get_one matches "dependency" with
    | Some dependency -> Ok dependency
    | None -> Error MissingDependency
  in
  match dependency, selection_of_matches ~default_selection matches, scope_of_matches matches with
  | Ok dependency, Ok selection, Ok scope ->
      let request: Riot_deps.add_request = Riot_deps.{ selection; scope; dependency } in
      let pm_session_id = Riot_model.Session_id.make () in
      let seen_registry_updates = HashSet.create () in
      (
        match Riot_deps.add
          ~on_event:(write_event ~mode ~pm_session_id ~seen_registry_updates)
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
  | Error err -> fail (CurrentDirUnavailable (path_error_message err))

let run_without_workspace = fun ~cwd matches ->
  match bootstrap_empty_workspace ~root:cwd with
  | Error err -> fail err
  | Ok () -> (
      match load_workspace ~root:cwd with
      | Error err -> fail err
      | Ok workspace -> run_request ~default_selection:Riot_deps.Workspace ~workspace ~cwd matches
    )
