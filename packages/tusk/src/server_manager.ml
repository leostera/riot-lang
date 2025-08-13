(** Server manager - Handles starting and managing the tusk server in the background *)

open Unix

(** Default server port *)
let default_port = 9876

(** PID file location *)
let pid_file () =
  let home = System.get_home () in
  Printf.sprintf "%s/.tusk/server.pid" home

(** Check if the server is running by trying to connect *)
let is_server_running () =
  try
    match Rpc_client.connect () with
    | Ok _ -> true
    | Error _ -> false
  with _ -> false

(** Start the server process in the background *)
let start_background () =
  (* Check if server is already running *)
  if is_server_running () then (
    Printf.printf "Server is already running on port %d\n" default_port;
    true
  ) else (
    (* Get the path to the tusk executable *)
    let tusk_exe = Sys.argv.(0) in
    
    (* Prepare to launch server as separate process *)
    let pid_path = pid_file () in
    System.mkdirp (Filename.dirname pid_path);
    
    (* Create pipes for stdout/stderr *)
    let devnull = Unix.openfile "/dev/null" [O_RDWR] 0o666 in
    
    (* Launch the server as a separate process *)
    let pid = Unix.create_process 
      tusk_exe 
      [| tusk_exe; "server"; "foreground" |]
      devnull devnull devnull
    in
    
    Unix.close devnull;
    
    (* Save PID *)
    System.write_file pid_path (string_of_int pid);
    
    (* Wait a moment for server to start *)
    Unix.sleep 2;
    
    (* Check if server started successfully *)
    if is_server_running () then (
      Printf.printf "✅ Server started in background (pid: %d) on port %d\n" pid default_port;
      true
    ) else (
      Printf.eprintf "❌ Failed to start server\n";
      (* Clean up PID file *)
      (try System.remove_file pid_path with _ -> ());
      false
    )
  )

(** Stop the background server *)
let stop_background () =
  try
    (* Try to send shutdown command via RPC *)
    match Rpc_client.connect () with
    | Ok _ ->
        match Rpc_client.call Rpc.Shutdown with
        | Ok _ ->
            Printf.printf "Server shutdown requested\n";
            (* Clean up PID file *)
            (try System.remove_file (pid_file ()) with _ -> ());
            true
        | Error msg ->
            Printf.eprintf "Failed to shutdown server: %s\n" msg;
            false
    | Error _ ->
        Printf.printf "Server is not running\n";
        (* Clean up stale PID file if it exists *)
        (try System.remove_file (pid_file ()) with _ -> ());
        false
  with _ ->
    Printf.printf "Server is not running\n";
    false

(** Ensure the server is running, starting it if necessary *)
let ensure_running () =
  if not (is_server_running ()) then
    start_background ()
  else
    true

(** Get server status *)
let status () =
  if is_server_running () then
    Printf.printf "✅ Server is running on port %d\n" default_port
  else
    Printf.printf "❌ Server is not running\n"