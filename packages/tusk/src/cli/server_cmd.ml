open Std
open Core
open Model
open Server

let command =
  let open ArgParser in
  let open Arg in
  command "server"
  |> about "Start or manage the tusk server"
  |> args
       [
         option "action"
         |> help "Action: start, stop, kill, status, or foreground"
         |> possible_values [ "start"; "stop"; "kill"; "status"; "foreground" ];
       ]

let get_daemon_info () =
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> failwith "Failed to get home directory"
  in
  let root_str = Path.to_string workspace.Workspace.root in
  let project_id = String.map (fun c -> if c = '/' then '-' else c) root_str in
  let daemon_path =
    Path.(home / Path.v ".tusk" / Path.v "projects" / Path.v project_id)
  in
  let pid_file = Path.(daemon_path / Path.v "server.pid") in
  let port_file = Path.(daemon_path / Path.v "server.port") in
  (workspace, daemon_path, pid_file, port_file)

let handle_start _matches =
  let workspace, _daemon_path, pid_file, _port_file = get_daemon_info () in
  match Fs.exists pid_file with
  | Ok true -> (
      match Fs.read_to_string pid_file with
      | Ok pid_str ->
          let pid = int_of_string (String.trim pid_str) in
          println "Server is already running (PID %d)" pid;
          Ok ()
      | Error _ ->
          println "Error: Failed to read PID file";
          Error (Failure "Failed to read PID file"))
  | Ok false | Error _ -> (
      println "Starting server in background...";
      match Server_manager.ensure_running ~workspace with
      | Ok _client ->
          println "Server started successfully";
          Ok ()
      | Error _ ->
          println "Error: Failed to start server";
          Error (Failure "Failed to start server"))

let handle_stop _matches =
  let _workspace, _daemon_path, pid_file, port_file = get_daemon_info () in
  match Fs.exists pid_file with
  | Ok true -> (
      match Fs.read_to_string pid_file with
      | Ok pid_str ->
          let pid = int_of_string (String.trim pid_str) in
          println "Stopping server process (PID %d)..." pid;
          let _ = Fs.remove_file pid_file in
          let _ = Fs.remove_file port_file in
          println "Server stopped (daemon files removed)";
          Ok ()
      | Error _ ->
          println "Error: Failed to read PID file";
          Error (Failure "Failed to read PID file"))
  | Ok false ->
      println "No server is running";
      Ok ()
  | Error _ ->
      println "Error: Failed to check for PID file";
      Error (Failure "Failed to check PID file")

let handle_kill _matches =
  let _workspace, _daemon_path, pid_file, port_file = get_daemon_info () in
  match Fs.exists pid_file with
  | Ok true -> (
      match Fs.read_to_string pid_file with
      | Ok pid_str ->
          let pid = int_of_string (String.trim pid_str) in
          println "Stopping server process (PID %d)..." pid;
          let _ = Fs.remove_file pid_file in
          let _ = Fs.remove_file port_file in
          println "Cleaned up daemon files";
          println
            "Note: The server process may still be running. It will exit on \
             its own.";
          Ok ()
      | Error _ ->
          println "Error: Failed to read PID file";
          Error (Failure "Failed to read PID file"))
  | Ok false ->
      println "No server is running (PID file not found)";
      Ok ()
  | Error _ ->
      println "Error: Failed to check for PID file";
      Error (Failure "Failed to check PID file")

let handle_status _matches =
  let _workspace, daemon_path, pid_file, port_file = get_daemon_info () in
  match Fs.exists pid_file with
  | Ok true -> (
      match (Fs.read_to_string pid_file, Fs.read_to_string port_file) with
      | Ok pid_str, Ok port_str -> (
          let pid = int_of_string (String.trim pid_str) in
          let port = int_of_string (String.trim port_str) in

          println "Server status:";
          println "  PID:  %d" pid;
          println "  Port: %d" port;
          println "  Logs: %s" (Path.to_string daemon_path);

          match Std.Net.TcpClient.connect ~host:"127.0.0.1" ~port with
          | Ok stream ->
              let _ = Std.Net.TcpClient.close stream in
              println "  State: Running ✓";
              Ok ()
          | Error _ ->
              println "  State: Not responding ✗";
              println "";
              println "Daemon files exist but server is not responding.";
              println "Run 'tusk server kill' to clean up.";
              Ok ()
          | _ ->
              println "Error: Failed to read daemon files";
              Error (Failure "Failed to read daemon files")))
  | Ok false ->
      println "No server is running";
      Ok ()
  | Error _ ->
      println "Error: Failed to check for daemon files";
      Error (Failure "Failed to check daemon files")

let handle_foreground _matches =
  println "🚀 Starting tusk server...";
  println "   Press Ctrl+C to stop\n";
  Tusk_server.start_with_listener ()

let run matches =
  let open ArgParser in
  match get_one matches "action" with
  | Some "start" -> handle_start matches
  | Some "stop" -> handle_stop matches
  | Some "kill" -> handle_kill matches
  | Some "status" -> handle_status matches
  | Some "foreground" -> handle_foreground matches
  | None -> handle_foreground matches
  | Some action ->
      println "Unknown server subcommand: %s" action;
      println "Available subcommands:";
      println "  tusk server            - Start server in foreground";
      println "  tusk server start      - Start server in background";
      println "  tusk server stop       - Stop background server";
      println "  tusk server kill       - Kill background server (force)";
      println "  tusk server status     - Check server status";
      Error (Failure "Invalid server subcommand")
