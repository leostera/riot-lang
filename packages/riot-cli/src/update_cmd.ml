open Std
open Std.Collections
open Std.Result.Syntax

type error =
  | InvalidPackageName of Riot_model.Package_name.error
  | UpdateFailed of Riot_deps.package_error

let out = eprintln

let no_workspace_message = "No riot.toml, so nothing to update"

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "update"
  |> about "Re-resolve the workspace graph, update locked package versions, and rewrite riot.lock"
  |> args
    [
      positional "package"
      |> required false
      |> multiple
      |> help "Optional package name to update; repeat by passing more names";
      flag "json"
      |> long "json"
      |> help "Render events as JSON";
    ]

let message = fun __tmp1 ->
  match __tmp1 with
  | InvalidPackageName error -> Riot_model.Package_name.error_message error
  | UpdateFailed error -> Package_error.message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let write_event = fun ~ui ~pm_session_id kind ->
  Riot_model.Event.create
    ~session_id:pm_session_id
    ~level:Riot_model.Event.Info
    (Riot_model.Event.Deps kind)
  |> fun event -> Ui.send ui event

let package_names_of_matches = fun matches ->
  let rec parse_all acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package :: rest ->
        let* package =
          Riot_model.Package_name.from_string package
          |> Result.map_err ~fn:(fun error -> InvalidPackageName error)
        in
        parse_all (package :: acc) rest
  in
  parse_all [] (ArgParser.get_many matches "package")

let run = fun ~workspace matches ->
  let mode = Ui.mode_of_json_flag (ArgParser.get_flag matches "json") in
  let ui = Ui.make ~mode () in
  let workspace_manager = Riot_model.Workspace_manager.create () in
  let pm_session_id = Riot_model.Session_id.make () in
  match package_names_of_matches matches with
  | Error err -> fail err
  | Ok packages -> (
      match Riot_deps.update
        ~on_event:(write_event ~ui ~pm_session_id)
        ~workspace_manager
        ~workspace
        ~request:Riot_deps.{ packages }
        () with
      | Ok () -> Ok ()
      | Error error -> fail (UpdateFailed error)
    )

let run_without_workspace = fun _matches ->
  out no_workspace_message;
  Ok ()
