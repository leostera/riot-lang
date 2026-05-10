open Std
open Std.Collections
open Std.Result.Syntax
open Riot_model

type error =
  | MissingDependency
  | ConflictingTarget
  | ConflictingScope
  | InvalidPackageName of Riot_model.Package_name.error
  | CurrentDirUnavailable of Path.error
  | RemoveFailed of Riot_deps.package_error

let out = eprintln

let no_workspace_message = "No riot.toml, so nothing to remove"

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "rm"
  |> about "Remove a dependency from a manifest section and refresh riot.lock"
  |> args
    [
      positional "dependency"
      |> multiple
      |> help "Dependency name to remove";
      option "package"
      |> short 'p'
      |> long "package"
      |> help "Edit a specific workspace package manifest";
      flag "workspace"
      |> long "workspace"
      |> help "Edit the workspace root manifest";
      flag "build"
      |> long "build"
      |> help "Remove from [build-dependencies]";
      flag "dev"
      |> long "dev"
      |> help "Remove from [dev-dependencies]";
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

let message = fun __tmp1 ->
  match __tmp1 with
  | MissingDependency -> "missing dependency name"
  | ConflictingTarget -> "cannot combine --workspace with --package"
  | ConflictingScope -> "cannot combine --build with --dev"
  | InvalidPackageName error -> Package_name.error_message error
  | CurrentDirUnavailable error ->
      "failed to determine current directory: " ^ path_error_message error
  | RemoveFailed error -> Package_error.message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let selection_of_matches = fun matches ->
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
  | (None, false) -> Ok Riot_deps.Current

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

let run = fun ~workspace matches ->
  let mode = Ui.mode_of_json_flag (ArgParser.get_flag matches "json") in
  let ui = Ui.make ~mode () in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let dependencies =
    match ArgParser.get_many matches "dependency" with
    | [] -> Error MissingDependency
    | dependencies ->
        let rec parse_all acc = fun __tmp1 ->
          match __tmp1 with
          | [] -> Ok (List.reverse acc)
          | dependency :: rest ->
              let* dependency =
                Package_name.from_string dependency
                |> Result.map_err ~fn:(fun error -> InvalidPackageName error)
              in
              parse_all (dependency :: acc) rest
        in
        parse_all [] dependencies
  in
  match (dependencies, selection_of_matches matches, scope_of_matches matches, Env.current_dir ()) with
  | (Ok dependencies, Ok selection, Ok scope, Ok cwd) ->
      let request: Riot_deps.remove_request = Riot_deps.{ selection; scope; dependencies } in
      let pm_session_id = Riot_model.Session_id.make () in
      (
        match Riot_deps.remove
          ~on_event:(write_event ~ui ~pm_session_id)
          ~workspace_manager
          ~workspace
          ~cwd
          ~request
          () with
        | Ok () -> Ok ()
        | Error error -> fail (RemoveFailed error)
      )
  | (Error err, _, _, _)
  | (_, Error err, _, _)
  | (_, _, Error err, _) -> fail err
  | (_, _, _, Error err) -> fail (CurrentDirUnavailable err)

let run_without_workspace = fun _matches ->
  out no_workspace_message;
  Ok ()
