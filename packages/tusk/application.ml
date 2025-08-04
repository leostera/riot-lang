(* Ox-inspired application architecture using Miniriot actors *)

type build_request = {
  workspace_path: string;
  packages: string list;
}

type build_result = 
  | Success of { built_modules: int; cached_modules: int; duration: float }
  | Failure of string

module BuildApplication = struct
  module Pid = Miniriot.Pid
  module Process = Miniriot.Process
  module Message = Miniriot.Message
  
  type t = {
    worker_pool_pid: Pid.t;
    coordinator_pid: Pid.t;
  }

  type Message.t += 
    | StartBuild of build_request
    | BuildComplete of build_result
    | WorkerReady of Pid.t
    | BuildTask of Workspace.source_file

  let worker_loop coordinator_pid () =
    let rec loop () =
      match Miniriot.receive () with
      | BuildTask source_file ->
          Printf.printf "[Worker %s] Building %s\n%!" 
            (Pid.to_string (Miniriot.self ())) source_file.Workspace.module_name;
          (* TODO: Integrate with build_server.ml for actual build *)
          Miniriot.sleep 0.1; (* Simulate work *)
          Miniriot.send coordinator_pid (BuildComplete (Success { built_modules = 1; cached_modules = 0; duration = 0.1 }));
          loop ()
      | _ -> loop ()
    in
    loop ();
    Process.Normal

  let coordinator_loop workspace_config build_server =
    let rec loop pending_files completed_count =
      match Miniriot.receive () with
      | StartBuild request ->
          Printf.printf "[Coordinator] Starting build for workspace: %s\n%!" request.workspace_path;
          let all_files = Workspace.discover_workspace_files request.workspace_path in
          Printf.printf "[Coordinator] Discovered %d source files\n%!" (List.length all_files);
          
          (* Send files to workers *)
          List.iter (fun source_file ->
            (* Simple round-robin to available workers for now *)
            (* Simple round-robin to available workers for now *)
            (* TODO: Implement proper worker pool management *)
            ()
          ) all_files;
          
          loop all_files 0
          
      | BuildComplete result ->
          let new_completed = completed_count + 1 in
          Printf.printf "[Coordinator] Build complete (%d/%d)\n%!" new_completed (List.length pending_files);
          if new_completed >= List.length pending_files then
            Printf.printf "[Coordinator] All builds complete!\n%!";
          loop pending_files new_completed
          
      | _ -> loop pending_files completed_count
    in
    loop [] 0;
    Process.Normal

  let start_workers count coordinator_pid =
    List.init count (fun i ->
      Miniriot.spawn (worker_loop coordinator_pid)
    )

  let start () =
    Printf.printf "[Application] Starting Tusk build system with Riot actors\n%!";
    
    (* Create build server for actual compilation *)
    let workspace_config = Workspace_config.parse_workspace_toml "tusk.toml" in
    let build_server = Build_server.create_build_server workspace_config.Workspace_config.ocaml_version in
    
    (* Start build coordinator first *)
    let coordinator_pid = Miniriot.spawn (fun () -> coordinator_loop workspace_config build_server) in
    Printf.printf "[Application] Started build coordinator\n%!";
    
    (* Start worker pool with coordinator PID *)
    let worker_pids = start_workers 4 coordinator_pid in
    Printf.printf "[Application] Started %d workers\n%!" (List.length worker_pids);
    
    { worker_pool_pid = List.hd worker_pids; coordinator_pid }

  let build_workspace app request =
    Miniriot.send app.coordinator_pid (StartBuild request);
    (* In a real implementation, we'd wait for completion *)
    Miniriot.sleep 2.0;
    Success { built_modules = 10; cached_modules = 5; duration = 1.5 }
end