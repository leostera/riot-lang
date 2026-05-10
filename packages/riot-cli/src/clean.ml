open Std
open Riot_model

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "clean"
  |> about "Run tracked build cache GC, or remove the build root with --force"
  |> args
    [
      flag "json"
      |> long "json"
      |> help "Emit machine-readable JSONL events";
      flag "force"
      |> long "force"
      |> help "Remove the entire build root instead of keeping it and running tracked cache GC";
    ]

let run = fun ~(workspace:Riot_model.Workspace.t) matches ->
  let mode = Ui.mode_of_json_flag (ArgParser.get_flag matches "json") in
  let ui = Ui.make ~mode () in
  let session_id = Riot_model.Session_id.make () in
  let on_event event = Ui.send ui (Riot_build.Event.cache_gc ~session_id event) in
  let on_waiting lock_path =
    Ui.send
      ui
      (Riot_build.Event.phase ~session_id (Riot_build.Event.BuildLockWaiting { lock_path }))
  in
  Riot_build.BuildLock.acquire_existing_lanes
    ~on_waiting
    ~target_dir_root:workspace.target_dir_root
    (fun () ->
      if ArgParser.get_flag matches "force" then
        match Riot_store.Cache_gc.force_clean_with_events ~workspace ~on_event with
        | Ok () -> Ok ()
        | Error error -> Error (Failure error)
      else
        match Riot_store.Cache_gc.clean_with_events ~workspace ~on_event with
        | Ok _ -> Ok ()
        | Error error -> Error (Failure error))
