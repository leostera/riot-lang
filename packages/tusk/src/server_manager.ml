open Std
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
    Path.join
      (Path.join home
         (Path.of_string ".tusk" |> Result.expect ~msg:"invalid path"))
      (Path.join
         (Path.of_string "daemons" |> Result.expect ~msg:"invalid path")
         (Path.of_string project_id |> Result.expect ~msg:"invalid path"))

  let daemon_exists ~workspace =
    let daemon_path = daemon_dir ~workspace in
    let pid_file =
      Path.join daemon_path
        (Path.of_string "server.pid" |> Result.expect ~msg:"invalid path")
    in
    let port_file =
      Path.join daemon_path
        (Path.of_string "server.port" |> Result.expect ~msg:"invalid path")
    in

    (* Check if daemon files exist and process is running *)
    let pid_path_str = Path.to_string pid_file in
    let port_path_str = Path.to_string port_file in
    match
      ( Miniriot.File.exists ~path:pid_path_str,
        Miniriot.File.exists ~path:port_path_str )
    with
    | true, true ->
        (* Read the PID and port *)
        let pid_content =
          Miniriot.File.read ~path:pid_path_str
          |> Result.expect ~msg:"Failed to read PID file"
        in
        let port_content =
          Miniriot.File.read ~path:port_path_str
          |> Result.expect ~msg:"Failed to read port file"
        in
        let pid = int_of_string (String.trim pid_content) in
        let port = int_of_string (String.trim port_content) in

        (* Check if process is still running *)
        if Command.is_pid_running pid then
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
        let pid_file =
          Path.join daemon_path
            (Path.of_string "server.pid" |> Result.expect ~msg:"invalid path")
        in
        let port_file =
          Path.join daemon_path
            (Path.of_string "server.port" |> Result.expect ~msg:"invalid path")
        in

        (* Ensure daemon directory exists *)
        let daemon_path_str = Path.to_string daemon_path in
        let () =
          if not (Miniriot.File.exists ~path:daemon_path_str) then
            let _ = Fs.mkdir daemon_path 0o755 in
            ()
        in

        (* Get tusk executable path - use the current executable *)
        let tusk_exe = Command.executable_name in

        (* Spawn the server in foreground mode *)
        (* Note: We use "server" "foreground" to run the server *)
        match Command.spawn ~cmd:tusk_exe ~args:[ "server"; "foreground" ] with
        | Ok process ->
            let pid = Command.pid process in
            let port = 9753 in
            (* Default port *)

            (* Write PID and port files *)
            let _ =
              Miniriot.File.write ~path:(Path.to_string pid_file)
                ~content:(string_of_int pid)
            in
            let _ =
              Miniriot.File.write ~path:(Path.to_string port_file)
                ~content:(string_of_int port)
            in

            (* Give the server a moment to start up *)
            Unix.sleepf 0.1;

            Ok { workspace; os_pid = pid; port; host = "127.0.0.1" }
        | Error err -> Error Error.ScanWorkspaceError)
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
              Miniriot.sleep 50;
              (* 50ms *)
              wait_server ~retries:(retries - 1))
      | Error e ->
          Miniriot.sleep 50;
          (* 50ms *)
          wait_server ~retries:(retries - 1)
  in
  wait_server ~retries:60 (* Wait up to 3 seconds *)
