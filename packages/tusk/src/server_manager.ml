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
    (* Fork the process *)
    match fork () with
    | 0 ->
        (* Child process *)
        (* Create a new session to detach from terminal *)
        ignore (setsid ());
        
        (* Close standard file descriptors *)
        close stdin;
        close stdout;
        close stderr;
        
        (* Redirect to /dev/null *)
        let devnull = openfile "/dev/null" [O_RDWR] 0o666 in
        dup2 devnull stdin;
        dup2 devnull stdout;
        dup2 devnull stderr;
        close devnull;
        
        (* Save PID *)
        let pid_path = pid_file () in
        System.mkdirp (Filename.dirname pid_path);
        System.write_file pid_path (string_of_int (getpid ()));
        
        (* Start the server *)
        ignore (Server.start_with_listener ());
        exit 0
    | pid ->
        (* Parent process *)
        (* Wait a moment for server to start *)
        Unix.sleep 1;
        
        (* Check if server started successfully *)
        if is_server_running () then (
          Printf.printf "✅ Server started in background (pid: %d) on port %d\n" pid default_port;
          true
        ) else (
          Printf.eprintf "❌ Failed to start server\n";
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