(** Server manager - Handles starting and managing the tusk server in the background *)

open Unix

(** Get project ID based on current working directory *)
let get_project_id () =
  let cwd = System.getcwd () in
  (* Hash the path to get a stable project ID *)
  let hash = Hasher.hash_string cwd in
  Hasher.to_string hash

(** Get daemon directory for this project *)
let daemon_dir () =
  let home = System.get_home () in
  let project_id = get_project_id () in
  Printf.sprintf "%s/.tusk/daemons/%s" home project_id

(** PID file location for this project *)
let pid_file () =
  Printf.sprintf "%s/server.pid" (daemon_dir ())

(** Port file location for this project *)
let port_file () =
  Printf.sprintf "%s/server.port" (daemon_dir ())

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
    (* Read the port from the port file *)
    let port = 
      try
        let port_path = port_file () in
        int_of_string (String.trim (System.read_file port_path))
      with _ -> 0
    in
    Printf.printf "Server is already running for this project on port %d\n" port;
    true
  ) else (
    (* Get the path to the tusk executable *)
    let tusk_exe = Sys.argv.(0) in
    
    (* Ensure daemon directory exists *)
    let daemon_path = daemon_dir () in
    System.mkdirp daemon_path;
    
    (* Save project info *)
    let project_info_path = Printf.sprintf "%s/project.info" daemon_path in
    let cwd = System.getcwd () in
    System.write_file project_info_path cwd;
    
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
    let pid_path = pid_file () in
    System.write_file pid_path (string_of_int pid);
    
    (* Wait a moment for server to start *)
    Unix.sleep 2;
    
    (* Check if server started successfully and read the port *)
    if is_server_running () then (
      let port = 
        try
          let port_path = port_file () in
          int_of_string (String.trim (System.read_file port_path))
        with _ -> 9876  (* fallback to default *)
      in
      Printf.printf "✅ Server started in background (pid: %d) on port %d for project %s\n" 
        pid port (get_project_id ());
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
    | Ok _ -> (
        match Rpc_client.call Rpc.Shutdown with
        | Ok _ ->
            Printf.printf "Server shutdown requested\n";
            (* Clean up PID file *)
            (try System.remove_file (pid_file ()) with _ -> ());
            true
        | Error msg ->
            Printf.eprintf "Failed to shutdown server: %s\n" msg;
            false)
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
  if is_server_running () then (
    let port = 
      try
        let port_path = port_file () in
        int_of_string (String.trim (System.read_file port_path))
      with _ -> 0
    in
    Printf.printf "✅ Server is running on port %d for project %s\n" 
      port (get_project_id ())
  ) else
    Printf.printf "❌ Server is not running for this project\n"