(** Server manager - Handles starting and managing the tusk server in the
    background *)

open Unix

(** Get project ID for a workspace *)
let get_project_id (workspace : Workspace.workspace) =
  (* Hash the workspace root to get a stable project ID *)
  let hash = Hasher.hash_string workspace.root in
  Hasher.to_string hash

(** Get daemon directory for a workspace *)
let daemon_dir_for_workspace (workspace : Workspace.workspace) =
  let home = System.get_home () in
  let project_id = get_project_id workspace in
  Printf.sprintf "%s/.tusk/daemons/%s" home project_id

(** PID file location for a workspace *)
let pid_file_for_workspace workspace = 
  Printf.sprintf "%s/server.pid" (daemon_dir_for_workspace workspace)

(** Port file location for a workspace *)
let port_file_for_workspace workspace = 
  Printf.sprintf "%s/server.port" (daemon_dir_for_workspace workspace)

(** Get current workspace *)
let current_workspace () =
  Workspace.scan ~root:(System.getcwd ())

(** Legacy functions that work with current directory *)
let daemon_dir () = daemon_dir_for_workspace (current_workspace ())
let pid_file () = pid_file_for_workspace (current_workspace ())
let port_file () = port_file_for_workspace (current_workspace ())

type daemon = {
  dir: string;
  os_pid: int;
  port: int;
}

(** Read daemon information for a workspace *)
let daemon (workspace : Workspace.workspace) =
  let dir = daemon_dir_for_workspace workspace in
  let pid_path = pid_file_for_workspace workspace in
  let port_path = port_file_for_workspace workspace in
  
  if Sys.file_exists pid_path && Sys.file_exists port_path then
    try
      let ic = open_in pid_path in
      let os_pid = int_of_string (input_line ic) in
      close_in ic;
      
      let ic = open_in port_path in
      let port = int_of_string (input_line ic) in
      close_in ic;
      
      Some { dir; os_pid; port }
    with _ -> None
  else None

(** Write daemon files for a workspace *)
let write_daemon (workspace : Workspace.workspace) ~port =
  let dir = daemon_dir_for_workspace workspace in
  let pid_path = pid_file_for_workspace workspace in
  let port_path = port_file_for_workspace workspace in
  
  (* Create directory if needed *)
  System.mkdirp dir;
  
  (* Write port file *)
  let oc = open_out port_path in
  output_string oc (string_of_int port);
  close_out oc;
  
  (* Write PID file *)
  let pid = Unix.getpid () in
  let oc = open_out pid_path in
  output_string oc (string_of_int pid);
  close_out oc

(** Remove daemon files for a workspace *)
let remove_daemon (workspace : Workspace.workspace) =
  let pid_path = pid_file_for_workspace workspace in
  let port_path = port_file_for_workspace workspace in
  (try Sys.remove port_path with _ -> ());
  (try Sys.remove pid_path with _ -> ())

(** Check if the server is running by checking process existence *)
let is_server_running () =
  let workspace = current_workspace () in
  match daemon workspace with
  | None -> false
  | Some info ->
      (* Check if the process is still alive *)
      try
        Unix.kill info.os_pid 0;
        true
      with Unix.Unix_error (Unix.ESRCH, _, _) ->
        (* Process doesn't exist, clean up stale files *)
        remove_daemon workspace;
        false

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
    true)
  else
    (* Get the path to the tusk executable *)
    let tusk_exe = Sys.argv.(0) in

    (* Ensure daemon directory exists *)
    let daemon_path = daemon_dir () in
    System.mkdirp daemon_path;

    (* Save project info *)
    let project_info_path = Printf.sprintf "%s/project.info" daemon_path in
    let cwd = System.getcwd () in
    System.write_file project_info_path cwd;

    (* Create log file for debugging *)
    let log_path = Printf.sprintf "%s/server.log" daemon_path in
    let log_fd = Unix.openfile log_path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644 in

    (* Use /dev/null for stdin, log file for stdout/stderr *)
    let devnull = Unix.openfile "/dev/null" [ O_RDONLY ] 0o666 in

    (* Launch the server as a separate process *)
    let pid =
      Unix.create_process tusk_exe
        [| tusk_exe; "server"; "foreground" |]
        devnull log_fd log_fd
    in

    Unix.close devnull;
    Unix.close log_fd;

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
        with _ -> 9876 (* fallback to default *)
      in
      Printf.printf
        "✅ Server started in background (pid: %d) on port %d for project %s\n"
        pid port (get_project_id (current_workspace ()));
      true)
    else (
      Printf.eprintf "❌ Failed to start server\n";
      (* Clean up PID file *)
      (try System.remove_file pid_path with _ -> ());
      false)

(** Stop the background server *)
let stop_background () =
  let workspace = current_workspace () in
  match daemon workspace with
  | None ->
      Printf.printf "Server is not running\n";
      false
  | Some info ->
      try
        (* Send SIGTERM to gracefully shutdown *)
        Unix.kill info.os_pid Sys.sigterm;
        Printf.printf "Server shutdown requested (pid: %d)\n" info.os_pid;
        (* Wait a moment for graceful shutdown *)
        Unix.sleep 1;
        (* Clean up daemon files *)
        remove_daemon workspace;
        true
      with Unix.Unix_error (Unix.ESRCH, _, _) ->
        (* Process doesn't exist, clean up stale files *)
        Printf.printf "Server was not running (stale PID file)\n";
        remove_daemon workspace;
        false

(** Ensure the server is running, starting it if necessary *)
let ensure_running () =
  if not (is_server_running ()) then start_background () else true

(** Get server status *)
let status () =
  if is_server_running () then
    let port =
      try
        let port_path = port_file () in
        int_of_string (String.trim (System.read_file port_path))
      with _ -> 0
    in
    Printf.printf "✅ Server is running on port %d for project %s\n" port
      (get_project_id (current_workspace ()))
  else Printf.printf "❌ Server is not running for this project\n"

(** Kill the background server forcefully *)
let kill_background () =
  let pid_path = pid_file () in
  let port_path = port_file () in

  (* Check if PID file exists *)
  if System.file_exists pid_path then (
    try
      let pid = int_of_string (String.trim (System.read_file pid_path)) in
      (* Kill the process with SIGKILL (kill -9) *)
      (try Unix.kill pid Sys.sigkill
       with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      Printf.printf "Killed server process (pid: %d)\n" pid;
      (* Clean up daemon files *)
      (try System.remove_file pid_path with _ -> ());
      (try System.remove_file port_path with _ -> ());
      true
    with _ ->
      Printf.printf "Failed to read PID file\n";
      (* Clean up stale files anyway *)
      (try System.remove_file pid_path with _ -> ());
      (try System.remove_file port_path with _ -> ());
      false)
  else (
    Printf.printf "Server is not running\n";
    (* Clean up any stale port file *)
    (try System.remove_file port_path with _ -> ());
    false)

