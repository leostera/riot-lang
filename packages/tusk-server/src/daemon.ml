open Std
open Tusk_model

type t = {
  workspace : Workspace.t;
  os_pid : int; (* Unix process ID *)
  port : int;
  host : string;
  config : Server_config.t;
}

let daemon_dir ~workspace =
  Log.debug
    ("[SERVER_MANAGER] daemon_dir for workspace project_id="
    ^ Workspace.project_id workspace);
  Tusk_model.Tusk_dirs.project_dir workspace

let daemon_exists ~workspace ~config =
  let daemon_path = daemon_dir ~workspace in
  let pid_file = Path.(daemon_path / Path.v "server.pid") in
  let port_file = Path.(daemon_path / Path.v "server.port") in

  Log.debug ("Checking for daemon files at " ^ Path.to_string daemon_path);

  (* Check if daemon files exist and process is running *)
  match (Fs.exists pid_file, Fs.exists port_file) with
  | Ok true, Ok true ->
      Log.debug "Found daemon files, reading PID and port";
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

      Log.debug
        ("Found daemon: pid=" ^ Int.to_string pid ^ ", port="
        ^ Int.to_string port);

      (* Check if server is actually running by trying to connect *)
      Log.debug ("Attempting to connect to 127.0.0.1:" ^ Int.to_string port);
      let is_server_running =
        match Net.TcpClient.connect ~host:"127.0.0.1" ~port with
        | Ok stream ->
            (* Connection successful - server is running *)
            Log.debug "Successfully connected to server";
            let _ = Net.TcpClient.close stream in
            true
        | Error e ->
            Log.debug
              ("Failed to connect: "
              ^ (match e with
                | Connection_refused -> "connection refused"
                | Closed -> "connection closed"
                | System_error io_err -> "system error: " ^ IO.error_message io_err));
            false
      in
      (* Reuse existing daemon if it's running - config doesn't matter *)
      if is_server_running then
        Some { workspace; os_pid = pid; port; host = "127.0.0.1"; config }
      else (
        (* Server not responding, clean up daemon files *)
        Log.debug "Server not responding, cleaning up daemon files";
        let _ = Fs.remove_file pid_file in
        let _ = Fs.remove_file port_file in
        None)
  | _ ->
      Log.debug "Daemon files not found";
      None

(** Start the daemon process *)
let of_workspace ~workspace ~config =
  (* 1. first get the workspace id and check if the right files exist in ~/.tusk/projects/<project-id> -- if they do, read and return those files *)
  match daemon_exists ~workspace ~config with
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
      let tusk_exe = System.executable_name in

      (* Spawn the server in foreground mode as a detached background process *)
      (* Open log files for redirection *)
      let stdout_log = Path.(daemon_path / Path.v "stdout.log") in
      let stderr_log = Path.(daemon_path / Path.v "stderr.log") in

      let stdout_file =
        Fs.File.open_append stdout_log
        |> Result.expect ~msg:"Failed to open stdout.log"
      in
      let stderr_file =
        Fs.File.open_append stderr_log
        |> Result.expect ~msg:"Failed to open stderr.log"
      in

      let stdout_fd = Fs.File.into_fd stdout_file in
      let stderr_fd = Fs.File.into_fd stderr_file in

      (* Configure stdio to redirect to log files *)
      let stdio =
        System.OsProcess.
          { stdin = `Null; stdout = `File stdout_fd; stderr = `File stderr_fd }
      in

      (* Build args list based on config *)
      let args = 
        if config.enable_codedb then
          [ "server"; "foreground" ]
        else
          [ "server"; "foreground"; "--no-code-server" ]
      in

      match
        System.OsProcess.spawn ~program:tusk_exe ~args ~stdio ()
      with
      | Ok process ->
          let pid = System.OsProcess.pid process in
          let port = Workspace.server_port workspace in

          (* File descriptors were consumed by into_fd, no need to close *)
          (* Write PID and port files *)
          let _ = Fs.write (Int.to_string pid) pid_file in
          let _ = Fs.write (Int.to_string port) port_file in

          Ok { workspace; os_pid = pid; port; host = "127.0.0.1"; config }
      | Error (`SpawnFailed msg) -> Error Error.ScanWorkspaceError)
