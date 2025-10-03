open Std
open Core
open Model
open Server

(** Execute the server command *)
let run args =
  (* Parse subcommand if provided *)
  let subcommand = if List.length args > 0 then List.nth args 0 else "" in
  match subcommand with
  | "start" ->
      (* Start server in background *)
      println "Server start not implemented yet";
      Ok ()
  | "stop" ->
      (* Stop background server *)
      println "Server stop not implemented yet";
      Ok ()
  | "kill" ->
      (* Kill background server forcefully *)
      let cwd =
        Env.current_dir ()
        |> Result.expect ~msg:"Failed to get current directory"
      in
      let workspace =
        Workspace_manager.scan cwd
        |> Result.expect ~msg:"Failed to scan workspace"
      in
      
      (* Get daemon directory *)
      let home =
        match Env.home_dir () with
        | Some h -> h
        | None -> failwith "Failed to get home directory"
      in
      let root_str = Path.to_string workspace.Workspace.root in
      let project_id = format "%08x" (Hashtbl.hash root_str) in
      let daemon_path =
        Path.(home / Path.v ".tusk" / Path.v "daemons" / Path.v project_id)
      in
      let pid_file = Path.(daemon_path / Path.v "server.pid") in
      
      (* Read PID and kill the process *)
      (match Fs.exists pid_file with
      | Ok true -> (
          match Fs.read_to_string pid_file with
          | Ok pid_str ->
              let pid = int_of_string (String.trim pid_str) in
              (try
                 Unix.kill pid Sys.sigterm;
                 println "Sent SIGTERM to server process (PID %d)" pid;
                 Kernel.Time.sleep 0.5;
                 (* Check if process is still alive *)
                 (try
                    Unix.kill pid 0;
                    (* Still alive, send SIGKILL *)
                    Unix.kill pid Sys.sigkill;
                    println "Sent SIGKILL to server process (PID %d)" pid
                  with Unix.Unix_error _ ->
                    println "Server process terminated gracefully")
               with Unix.Unix_error (Unix.ESRCH, _, _) ->
                 println "Server process (PID %d) not found" pid);
              
              (* Clean up daemon files *)
              let port_file = Path.(daemon_path / Path.v "server.port") in
              let _ = Fs.remove_file pid_file in
              let _ = Fs.remove_file port_file in
              println "Cleaned up daemon files";
              Ok ()
          | Error _ ->
              println "Error: Failed to read PID file";
              Error (Failure "Failed to read PID file"))
      | Ok false ->
          println "No server is running (PID file not found)";
          Ok ()
      | Error _ ->
          println "Error: Failed to check for PID file";
          Error (Failure "Failed to check PID file"))
  | "status" ->
      (* Check server status *)
      println "Server status not implemented yet";
      Ok ()
  | "" | "foreground" ->
      (* Default: Run server in foreground *)
      println "🚀 Starting tusk server...";
      println "   Press Ctrl+C to stop\n";
      Tusk_server.start_with_listener ()
  | _ ->
      println "Unknown server subcommand: %s" subcommand;
      println "Available subcommands:";
      println "  tusk server            - Start server in foreground";
      println "  tusk server start      - Start server in background";
      println "  tusk server stop       - Stop background server";
      println "  tusk server kill       - Kill background server (force)";
      println "  tusk server status     - Check server status";
      Error (Failure "Invalid server subcommand")
