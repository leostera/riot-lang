open Std

open Core
(** Build command implementation *)

open Model
open Server

(** Parse command line arguments for build command *)
let parse_build_args args start_idx =
  let rec parse idx package =
    if idx >= List.length args then package
    else
      match List.nth args idx with
      | "-p" when idx + 1 < List.length args ->
          parse (idx + 2) (Some (List.nth args (idx + 1)))
      | _ ->
          Printf.eprintf "Warning: Unknown argument '%s'\n" (List.nth args idx);
          parse (idx + 1) package
  in
  parse start_idx None

(** Execute the build command *)
let build_command package_opt =
  (* Make sure we have a valid workspace *)
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd
    |> Result.expect
         ~msg:"Failed to scan workspace. Is this a valid tusk project?"
  in

  (* Ensure server is running *)
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
  (* Track packages we've already displayed to avoid duplicates *)
  let displayed_packages = Hashtbl.create 32 in
  let result =
    Client.build_streaming client request (fun event ->
        match event with
        | Client.BuildStarted session_id -> ()
        | Client.BuildEvent event ->
            (* Only display package events once *)
            let should_display =
              match event.kind with
              | CacheHit { package; _ } | CacheMiss { package; _ } ->
                  if Hashtbl.mem displayed_packages package then false
                  else (
                    Hashtbl.add displayed_packages package ();
                    true)
              | PackageComplete { package; _ } -> (
                  (* Always show failures, but not successes (already shown as Compiling) *)
                  match event.kind with
                  | PackageComplete { success = false; _ } -> true
                  | _ -> false)
              | _ -> true
            in
            if should_display then
              let formatted = Event_formatter.format event in
              if formatted <> "" then Printf.printf "%s\n%!" formatted
        | Client.BuildFinished _ -> ())
    |> Result.expect ~msg:"Build failed"
  in
  Client.close client;

  (* Print final result *)
  match result with
  | Client.BuildFinished (Ok ()) -> Ok ()
  | Client.BuildFinished (Error msg) ->
      Printf.eprintf "error: build failed: %s\n" msg;
      Error (Failure "Build failed")
  | Client.BuildStarted _ | Client.BuildEvent _ ->
      (* These should not happen as final result, but handle just in case *)
      Error (Failure "Unexpected response from server")

let run args =
  let package_opt = parse_build_args args 0 in
  build_command package_opt
