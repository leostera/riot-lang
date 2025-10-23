open Std
open Tusk_model
open Tusk_model
open Tusk_server

let command =
  let open ArgParser in
  let open Arg in
  command "build" |> about "Build packages"
  |> args
       [
         option "package" |> short 'p' |> long "package"
         |> help "Build only the specified package";
       ]

let build_command package_opt =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd
    |> Result.expect
         ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in

  let client =
    Tusk_server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  let request =
    match package_opt with
    | Some pkg -> Tusk_client.BuildPackage pkg
    | None -> Tusk_client.BuildAll
  in
  let displayed_packages = Hashtbl.create 32 in
  let result =
    Tusk_client.build_streaming client request (fun event ->
        match event with
        | Tusk_client.BuildStarted session_id -> ()
        | Tusk_client.BuildEvent event ->
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; _ } -> (
                  match event.kind with
                  | PackageComplete { success = false; _ } -> true
                  | _ -> false)
              | _ -> true
            in
            if should_display then
              let formatted = Event_formatter.format event in
              if formatted <> "" then println "%s" formatted
        | Tusk_client.BuildFinished _ -> ())
    |> Result.expect ~msg:"Build failed"
  in
  Tusk_client.close client;

  match result with
  | Tusk_client.BuildFinished (Ok ()) -> Ok ()
  | Tusk_client.BuildFinished (Error msg) ->
      println "error: build failed: %s" msg;
      Error (Failure "Build failed")
  | Tusk_client.BuildStarted _ | Tusk_client.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  build_command package_opt
