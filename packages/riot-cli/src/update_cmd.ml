open Std
open Std.Collections

type error =
  | UpdateFailed of Riot_deps.package_error

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "update"
    |> about "Re-resolve the workspace graph, update locked package versions, and rewrite riot.lock"
    |> args [ flag "json" |> long "json" |> help "Render events as JSON"; ]

let message = function
  | UpdateFailed error -> Riot_deps.package_error_message error

let fail = fun err ->
  out ("\027[1;31mError\027[0m: " ^ message err);
  Error (Failure (message err))

let write_event = fun ~mode ~pm_session_id ~seen_registry_updates kind ->
  Riot_model.Event.create ~session_id:pm_session_id ~level:Riot_model.Event.Info kind
  |> Build.write_pm_event ~mode ~seen_registry_updates

let run = fun ~workspace matches ->
  let mode =
    if ArgParser.get_flag matches "json" then
      Build.Json
    else
      Build.Human
  in
  let pm_session_id = Riot_model.Session_id.make () in
  let seen_registry_updates = HashSet.create () in
  match Riot_deps.update
    ~on_event:(write_event ~mode ~pm_session_id ~seen_registry_updates)
    ~workspace
    () with
  | Ok () -> Ok ()
  | Error error -> fail (UpdateFailed error)
