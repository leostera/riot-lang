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
    Printf.sprintf "%08x" (Hashtbl.hash root_str)

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

    (* Check if daemon files exist and process is running *)
    match (Fs.exists pid_file, Fs.exists port_file) with
    | Ok true, Ok true ->
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

        (* Check if process is still running by sending signal 0 *)
        let is_pid_running =
          let cmd = Command.make ~args:["-0"; string_of_int pid] "kill" in
          match Command.status cmd with
          | Ok 0 -> true (* Process exists *)
          | Ok _ | Error _ -> false (* Process doesn't exist or error checking *)
        in
        if is_pid_running then
          Some { workspace; os_pid = pid; port; host = "127.0.0.1" }
        else
          (* Process died, clean up *)
          let _ = Fs.remove_file pid_file in
          let _ = Fs.remove_file port_file in
          None
    | _ -> None

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

        (* Spawn the server in foreground mode *)
        (* Note: We use "server" "foreground" to run the server *)
        let stdio =
          Kernel.System.OsProcess.
            { stdin = `Null; stdout = `Inherit; stderr = `Inherit }
        in
        match
          Kernel.System.OsProcess.spawn ~program:tusk_exe
            ~args:[ "server"; "foreground" ] ~stdio ()
        with
        | Ok process ->
            let pid = Kernel.System.OsProcess.pid process in
            let port = 9753 in
            (* Default port *)

            (* Write PID and port files *)
            let _ = Fs.write (string_of_int pid) pid_file in
            let _ = Fs.write (string_of_int port) port_file in

            (* Give the server a moment to start up *)
            Kernel.Time.sleep 0.1;

            Ok { workspace; os_pid = pid; port; host = "127.0.0.1" }
        | Error (`SpawnFailed msg) -> Error Error.ScanWorkspaceError)
end

let ensure_running ~workspace =
  (* 1. Get a daemon for the workspace *)
  let daemon =
    Daemon.of_workspace ~workspace
    |> Result.expect ~msg:"Failed to get daemon info from workspace"
  in
  (* 2. Wait for server to be ready *)
  let rec wait_server ~retries =
    if retries <= 0 then Error Error.ScanWorkspaceError
    else
      match Tusk_jsonrpc.Client.create ~host:daemon.host ~port:daemon.port with
      | Ok client -> (
          (* Try to ping to make sure it's really ready *)
          match Tusk_jsonrpc.Client.ping client with
          | Ok _ -> Ok client
          | Error e ->
              Tusk_jsonrpc.Client.close client;
              Kernel.Time.sleep 0.05;
              (* 50ms *)
              wait_server ~retries:(retries - 1))
      | Error e ->
          Kernel.Time.sleep 0.05;
          (* 50ms *)
          wait_server ~retries:(retries - 1)
  in
  wait_server ~retries:60 (* Wait up to 3 seconds *)
