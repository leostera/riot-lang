open Std
open Tusk_model

(** Server manager - Handles starting and managing the tusk server in the
    background *)

let ensure_running ~(workspace : Workspace.t) =
  Log.debug
    ("ensure_running: Getting daemon for workspace root="
    ^ Path.to_string workspace.root);
  (* 1. Get a daemon for the workspace *)
  let daemon =
    Daemon.of_workspace ~workspace
    |> Result.expect ~msg:"Failed to get daemon info from workspace"
  in
  Log.debug
    ("ensure_running: Got daemon at " ^ daemon.host ^ ":"
    ^ Int.to_string daemon.port ^ " (PID " ^ Int.to_string daemon.os_pid ^ ")");

  (* 2. Wait for server to be ready *)
  let rec wait_server ~retries ~(daemon : Daemon.t) =
    Log.debug
      ("wait_server: Attempting connection to " ^ daemon.host ^ ":"
      ^ Int.to_string daemon.port ^ " (retries left: "
      ^ Int.to_string retries ^ ")");
    if retries <= 0 then (
      Log.warn "Failed to connect to server after 10 retries";
      Log.warn
        ("Server (PID " ^ Int.to_string daemon.os_pid
        ^ ") not responding, cleaning up and restarting...");

       (* Clean up stale daemon files and kill existing process *)
       let daemon_path = Daemon.daemon_dir ~workspace:daemon.workspace in
       let pid_file = Path.(daemon_path / Path.v "server.pid") in
       let port_file = Path.(daemon_path / Path.v "server.port") in
       let _ = Fs.remove_file pid_file in
       let _ = Fs.remove_file port_file in
       
       (* Kill the existing daemon process if it's still running *)
       (* TODO: Re-enable process signaling when System.OsProcess.signal is available *)
       (* (match Fs.read_to_string pid_file with
        | Ok pid_str when pid_str <> "" ->
            let pid = int_of_string (String.trim pid_str) in
            (match System.OsProcess.signal pid System.sigkill with
             | Ok () -> Log.debug ("Killed existing daemon process " ^ Int.to_string pid)
             | Error e -> Log.debug ("Failed to kill process " ^ Int.to_string pid ^ ": " ^ e))
        | _ -> Log.debug "No PID file found, skipping process kill"
       ); *)
       Log.debug "Skipping process kill (System.OsProcess.signal not available)";

       (* Try to start a new daemon *)
       match Daemon.of_workspace ~workspace:daemon.workspace with
       | Ok new_daemon ->
           println
             ("Started new tusk server (PID " ^ Int.to_string new_daemon.os_pid
             ^ ")...");
           wait_server ~retries:10 ~daemon:new_daemon
       | Error e ->
           Log.error "Failed to restart server";
           Error e
      | Error e ->
          Log.error "Failed to restart server";
          Error e)
    else
      match Tusk_client.create ~host:daemon.host ~port:daemon.port with
      | Ok client -> (
          Log.debug "Created client, testing with ping";
          (* Try to ping to make sure it's really ready *)
          match Tusk_client.ping client with
          | Ok _ ->
              Log.debug "Ping successful!";
              Ok client
          | Error e ->
              Log.debug ("Ping failed: " ^ e ^ ", retrying...");
              Tusk_client.close client;
          Kernel.Time.sleep 0.05;
              (* 50ms *)
              wait_server ~retries:(retries - 1) ~daemon)
      | Error e ->
          Log.info ("Failed to create client: " ^ e ^ " (retrying in 50ms)");
          Kernel.Time.sleep 0.05;
          (* 50ms *)
          wait_server ~retries:(retries - 1) ~daemon
  in
  wait_server ~retries:10 ~daemon (* Wait up to 0.5 seconds *)
