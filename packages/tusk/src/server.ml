(** Build server - Miniriot process that orchestrates builds *)

open Miniriot

(** Extend Miniriot's message type with our custom messages *)
type Message.t += 
  | ScanWorkspace
  | BuildAll
  | BuildPackage of string
  | Shutdown

(** Server state *)
type state = {
  workspace : Workspace.workspace option;
  build_graph : Build_graph.t option;
}

(** Handle scanning the workspace *)
let handle_scan_workspace state =
  let root = Sys.getcwd () in
  Printf.printf "[Server] Scanning workspace from: %s\n" root;
  
  let workspace = Workspace.scan ~root in
  let build_graph = 
    if workspace.packages = [] then None
    else Some (Build_graph.create workspace)
  in
  
  (* Print the build graph if we have one *)
  (match build_graph with
  | None -> Printf.printf "[Server] No packages found in workspace\n"
  | Some graph -> Build_graph.print graph);
  
  { workspace = Some workspace; build_graph }

(** Main server loop *)
let rec server_loop state =
  match receive () with
  | ScanWorkspace ->
      let new_state = handle_scan_workspace state in
      server_loop new_state
      
  | BuildAll ->
      Printf.printf "[Server] BuildAll command received\n";
      (match state.build_graph with
      | None -> 
          Printf.printf "[Server] No build graph available. Run ScanWorkspace first.\n"
      | Some graph ->
          Printf.printf "[Server] Building all packages...\n";
          let sorted = Build_graph.topological_sort graph in
          List.iter (fun node ->
            Printf.printf "[Server] Would build package: %s\n" node.Build_graph.package.name
          ) sorted);
      server_loop state
      
  | BuildPackage name ->
      Printf.printf "[Server] BuildPackage %s command received\n" name;
      server_loop state
      
  | Shutdown ->
      Printf.printf "[Server] Shutting down...\n";
      Process.Normal
      
  | _ ->
      (* Ignore unknown messages *)
      server_loop state

(** Start the build server *)
let start () =
  let initial_state = { workspace = None; build_graph = None } in
  spawn (fun () -> 
    Printf.printf "[Server] Build server started (pid: %s)\n" 
      (Pid.to_string (self ()));
    server_loop initial_state
  )