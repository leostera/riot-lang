open Std
open Core
open Model
open Server

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
    Server.Server_manager.ensure_running ~workspace
    |> Result.expect ~msg:"Failed to start or connect to tusk server"
  in

  let open Tusk_jsonrpc in
  let request =
    match package_opt with
    | Some pkg -> Client.BuildPackage pkg
    | None -> Client.BuildAll
  in
  let displayed_packages = Hashtbl.create 32 in
  let result =
    Client.build_streaming client request (fun event ->
        match event with
        | Client.BuildStarted session_id -> ()
        | Client.BuildEvent event ->
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
        | Client.BuildFinished _ -> ())
    |> Result.expect ~msg:"Build failed"
  in
  Client.close client;

  match result with
  | Client.BuildFinished (Ok ()) -> Ok ()
  | Client.BuildFinished (Error msg) ->
      println "error: build failed: %s" msg;
      Error (Failure "Build failed")
  | Client.BuildStarted _ | Client.BuildEvent _ ->
      Error (Failure "Unexpected response from server")

let run matches =
  let open ArgParser in
  let package_opt = get_one matches "package" in
  build_command package_opt
