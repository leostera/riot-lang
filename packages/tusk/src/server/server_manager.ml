open Std
open Model

(** Server manager - Handles starting and managing the tusk server in the
    background *)

module Daemon = struct
  type t = {
    workspace : Workspace.t;
    os_pid : int; (* Unix process ID *)
    port : int;
    host : string;
  }

  let get_project_id workspace =
    (* Use workspace root path as a simple project ID *)
    let root_str = Path.to_string workspace.Workspace.root in
    (* Hash the path to get a shorter ID *)
    format "%08x" (Hashtbl.hash root_str)

  let daemon_dir ~workspace =
    let home =
      match Env.home_dir () with
      | Some h -> h
      | None -> failwith "Failed to get home directory"
    in
    let project_id = get_project_id workspace in
    Path.(home / Path.v ".tusk" / Path.v "daemons" / Path.v project_id)

  let daemon_exists ~workspace =
    let daemon_path = daemon_dir ~workspace in
    let pid_file = Path.(daemon_path / Path.v "server.pid") in
    let port_file = Path.(daemon_path / Path.v "server.port") in

    Std.Log.debug "Checking for daemon files at %s" (Path.to_string daemon_path);

    (* Check if daemon files exist and process is running *)
    match (Fs.exists pid_file, Fs.exists port_file) with
    | Ok true, Ok true ->
        Std.Log.debug "Found daemon files, reading PID and port";
        (* Read the PID and port *)
        let pid_content =
          Fs.read_to_string pid_file
          |> Result.expect ~msg:"Failed to read PID file"
        in
        let port_content =
          Fs.read_to_string port_file
          |> Result.expect ~msg:"Failed to read port file"
        in
        let pid = int_of_string (String.trim pid_content) in
        let port = int_of_string (String.trim port_content) in

        Std.Log.debug "Found daemon: pid=%d, port=%d" pid port;

        (* Check if server is actually running by trying to connect *)
        Std.Log.debug "Attempting to connect to 127.0.0.1:%d" port;
        let is_server_running =
          match Std.Net.TcpClient.connect ~host:"127.0.0.1" ~port with
          | Ok stream ->
              (* Connection successful - server is running *)
              Std.Log.debug "Successfully connected to server";
              let _ = Std.Net.TcpClient.close stream in
              true
          | Error e ->
              Std.Log.debug "Failed to connect: %s"
                (match e with
                | `Connection_refused -> "connection refused"
                | `Closed -> "connection closed"
                | `System_error msg -> format "system error: %s" msg);
              false
        in
        if is_server_running then
          Some { workspace; os_pid = pid; port; host = "127.0.0.1" }
        else (
          (* Server not responding, clean up daemon files *)
          Std.Log.debug "Server not responding, cleaning up daemon files";
          let _ = Fs.remove_file pid_file in
          let _ = Fs.remove_file port_file in
          None)
    | _ ->
        Std.Log.debug "Daemon files not found";
        None

  (** Start the daemon process *)
  let of_workspace ~workspace =
    (* 1. first get the workspace id and check if the right files exist in ~/.tusk/daemons/<project-id> -- if they do, read and return those files *)
    match daemon_exists ~workspace with
    | Some daemon -> Ok daemon
    | None -> (
        (* 2. Spawn a new server process *)
        let daemon_path = daemon_dir ~workspace in
        let pid_file = Path.(daemon_path / Path.v "server.pid") in
        let port_file = Path.(daemon_path / Path.v "server.port") in

        (* Ensure daemon directory exists *)
        (match Fs.exists daemon_path with
        | Ok false ->
            let _ = Fs.create_dir_all daemon_path in
            ()
        | _ -> ());

        (* Get tusk executable path - use the current executable *)
        let tusk_exe = Kernel.System.executable_name in

        (* Spawn the server in foreground mode as a detached background process *)
        (* Open log files for redirection *)
        let stdout_log = Path.(daemon_path / Path.v "stdout.log") in
        let stderr_log = Path.(daemon_path / Path.v "stderr.log") in
        
        let stdout_fd =
          Kernel.Fs.File.openfile (Path.to_string stdout_log)
            [ Kernel.Fs.File.O_WRONLY; Kernel.Fs.File.O_CREAT; Kernel.Fs.File.O_TRUNC ]
            0o644
          |> Result.expect ~msg:"Failed to open stdout.log"
        in
        let stderr_fd =
          Kernel.Fs.File.openfile (Path.to_string stderr_log)
            [ Kernel.Fs.File.O_WRONLY; Kernel.Fs.File.O_CREAT; Kernel.Fs.File.O_TRUNC ]
            0o644
          |> Result.expect ~msg:"Failed to open stderr.log"
        in
        
        (* Configure stdio to redirect to log files *)
        let stdio =
          Kernel.System.OsProcess.{
            stdin = `Null;
            stdout = `File stdout_fd;
            stderr = `File stderr_fd;
          }
        in
        
        match
          Kernel.System.OsProcess.spawn ~program:tusk_exe
            ~args:[ "server"; "foreground" ] ~stdio ()
        with
        | Ok process ->
            let pid = Kernel.System.OsProcess.pid process in
            let port = 9753 in
            (* Default port *)
            
            (* Close file descriptors in parent - child has inherited them *)
            let _ = Kernel.Fs.File.close_fd stdout_fd in
            let _ = Kernel.Fs.File.close_fd stderr_fd in

            (* Write PID and port files *)
            let _ = Fs.write (string_of_int pid) pid_file in
            let _ = Fs.write (string_of_int port) port_file in

            (* Give the server more time to start up since it's detached *)
            Kernel.Time.sleep 0.5;

            Ok { workspace; os_pid = pid; port; host = "127.0.0.1" }
        | Error (`SpawnFailed msg) -> Error Error.ScanWorkspaceError)
end

let ensure_running ~workspace =
  Std.Log.debug "ensure_running: Getting daemon for workspace";
  (* 1. Get a daemon for the workspace *)
  let daemon =
    Daemon.of_workspace ~workspace
    |> Result.expect ~msg:"Failed to get daemon info from workspace"
  in
  Std.Log.debug "ensure_running: Got daemon at %s:%d" daemon.host daemon.port;

  (* 2. Wait for server to be ready *)
  let rec wait_server ~retries =
    if retries <= 0 then (
      Std.Log.error "Failed to connect to server after 60 retries";
      Error Error.ScanWorkspaceError)
    else
      match Tusk_jsonrpc.Client.create ~host:daemon.host ~port:daemon.port with
      | Ok client -> (
          Std.Log.debug "Created client, testing with ping";
          (* Try to ping to make sure it's really ready *)
          match Tusk_jsonrpc.Client.ping client with
          | Ok _ ->
              Std.Log.debug "Ping successful!";
              Ok client
          | Error e ->
              Std.Log.debug "Ping failed: %s, retrying..." e;
              Tusk_jsonrpc.Client.close client;
              Kernel.Time.sleep 0.05;
              (* 50ms *)
              wait_server ~retries:(retries - 1))
      | Error e ->
          Std.Log.debug "Failed to create client: %s, retrying..." e;
          Kernel.Time.sleep 0.05;
          (* 50ms *)
          wait_server ~retries:(retries - 1)
  in
  wait_server ~retries:60 (* Wait up to 3 seconds *)
