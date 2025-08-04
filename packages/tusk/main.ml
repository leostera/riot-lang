


(* PackageConfig module - not used yet, will implement when needed *)
module PackageConfig = struct
  type dependency = {
    name: string;
    workspace: bool;
    path: string option;
  }

  type t = {
    name: string;
    version: string;
    dependencies: dependency list;
  }
  
  (* TODO: implement when we need package-specific configuration parsing *)
end

module ProjectId = struct
  let tusk_daemons_dir () =
    let home = Sys.getenv "HOME" in
    let tusk_dir = Filename.concat home ".tusk" in
    Filename.concat tusk_dir "daemons"

  let compute_project_id project_path =
    (* Simple hash of project path for now *)
    let hash_cmd = Printf.sprintf "echo '%s' | shasum -a 256 | cut -d' ' -f1" project_path in
    let ic = Unix.open_process_in hash_cmd in
    let hash = input_line ic in
    ignore (Unix.close_process_in ic);
    String.trim hash

  let get_project_id () =
    let cwd = Sys.getcwd () in
    compute_project_id cwd

  let get_pid_file_path project_id =
    let daemons_dir = tusk_daemons_dir () in
    (* Ensure daemons directory exists *)
    (try Unix.mkdir (Filename.dirname daemons_dir) 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    (try Unix.mkdir daemons_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
    Filename.concat daemons_dir (project_id ^ ".pid")

  let is_daemon_running project_id =
    let pid_file = get_pid_file_path project_id in
    if Sys.file_exists pid_file then
      try
        let ic = open_in pid_file in
        let pid_str = input_line ic in
        close_in ic;
        let pid = int_of_string (String.trim pid_str) in
        (* Check if process is still running *)
        Unix.kill pid 0;
        true
      with _ -> 
        (* PID file exists but process is dead, clean up *)
        (try Sys.remove pid_file with _ -> ());
        false
    else false

  let write_pid_file project_id pid =
    let pid_file = get_pid_file_path project_id in
    let oc = open_out pid_file in
    Printf.fprintf oc "%d\n" pid;
    close_out oc

  let remove_pid_file project_id =
    let pid_file = get_pid_file_path project_id in
    try Sys.remove pid_file with _ -> ()
end

module RpcProtocol = struct
  type request = 
    | Build
    | Status
    | Stop

  type response =
    | BuildResult of { success: bool; message: string }
    | StatusResult of { running: bool; project_id: string }
    | StopResult of { success: bool }

  let serialize_request = function
    | Build -> "BUILD\n"
    | Status -> "STATUS\n" 
    | Stop -> "STOP\n"

  let parse_request line =
    match String.trim line with
    | "BUILD" -> Some Build
    | "STATUS" -> Some Status
    | "STOP" -> Some Stop
    | _ -> None

  let serialize_response = function
    | BuildResult { success; message } -> 
        Printf.sprintf "BUILD_RESULT:%b:%s\n" success message
    | StatusResult { running; project_id } ->
        Printf.sprintf "STATUS_RESULT:%b:%s\n" running project_id
    | StopResult { success } ->
        Printf.sprintf "STOP_RESULT:%b\n" success
end

let usage_msg = "tusk - OCaml build system\n\nUsage: tusk [COMMAND]\n\nCommands:\n  build    Build all packages (starts daemon if needed)\n  start    Start the tusk daemon\n  stop     Stop the tusk daemon\n  bsp      Start Build Server Protocol server\n  mcp      Start MCP protocol server\n  clean    Clean build artifacts\n  help     Show this help message"

let start_daemon_command () =
  let project_id = ProjectId.get_project_id () in
  Printf.printf "Starting tusk daemon for project %s...\n%!" project_id;
  
  if ProjectId.is_daemon_running project_id then (
    Printf.printf "Daemon already running for this project\n%!";
    0
  ) else (
    Printf.printf "TODO: Implement daemon server\n%!";
    Printf.printf "Would start daemon and write PID to: %s\n%!" (ProjectId.get_pid_file_path project_id);
    0
  )

let stop_daemon_command () =
  let project_id = ProjectId.get_project_id () in
  Printf.printf "Stopping tusk daemon for project %s...\n%!" project_id;
  
  if not (ProjectId.is_daemon_running project_id) then (
    Printf.printf "No daemon running for this project\n%!";
    0
  ) else (
    Printf.printf "TODO: Send stop RPC to daemon\n%!";
    ProjectId.remove_pid_file project_id;
    Printf.printf "Daemon stopped\n%!";
    0
  )

let build_command () =
  let project_id = ProjectId.get_project_id () in
  Printf.printf "🔨 Building project %s with Ox architecture...\n%!" project_id;
  
  (* Initialize Miniriot runtime and start application *)
  let module Process = Miniriot.Process in
  Miniriot.run ~main:(fun () ->
    let app = Application.BuildApplication.start () in
    
    let build_request = Application.{
      workspace_path = "tusk.toml";
      packages = ["gluon"; "miniriot"; "minitusk"; "tusk"];
    } in
    
    match Application.BuildApplication.build_workspace app build_request with
    | Application.Success { built_modules; cached_modules; duration } ->
        Printf.printf "✓ Build successful: %d built, %d cached (%.2fs)\n%!" 
          built_modules cached_modules duration;
        Process.Normal
    | Application.Failure error ->
        Printf.printf "✗ Build failed: %s\n%!" error;
        Process.Exception (Failure error)
  )

let bsp_command () =
  Printf.printf "🔌 Starting Build Server Protocol server...\n%!";
  Printf.printf "TODO: Implement BSP server endpoint\n%!";
  0

let mcp_command () =
  Printf.printf "🔌 Starting MCP protocol server...\n%!";
  Printf.printf "TODO: Implement MCP server endpoint\n%!";
  0

let clean_command () =
  Printf.printf "🧹 Cleaning build artifacts...\n%!";
  let result = Unix.system "rm -rf ./target/sandbox" in
  match result with
  | Unix.WEXITED code -> 
      Printf.printf "Build artifacts cleaned!\n%!";
      code
  | _ -> 1

let help_command () =
  Printf.printf "%s\n%!" usage_msg;
  0

let main () =
  let args = Sys.argv in
  let argc = Array.length args in
  
  if argc < 2 then (
    Printf.eprintf "Error: No command specified\n\n%s\n%!" usage_msg;
    exit 1
  );
  
  let command = args.(1) in
  let exit_code = match command with
    | "build" -> build_command ()
    | "start" -> start_daemon_command ()
    | "stop" -> stop_daemon_command ()
    | "bsp" -> bsp_command ()
    | "mcp" -> mcp_command ()
    | "clean" -> clean_command ()
    | "help" | "--help" | "-h" -> help_command ()
    | _ ->
        Printf.eprintf "Error: Unknown command '%s'\n\n%s\n%!" command usage_msg;
        1
  in
  
  exit exit_code

let () = main ()