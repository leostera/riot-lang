open Std

type config = {
  package_name : string;
  watch_paths : Path.t list;
  binary_path : Path.t;
  binary_args : string list;
}

(* Connect to tusk server - default port from tusk-server *)
let connect_to_tusk () =
  Tusk_client.create ~host:"127.0.0.1" ~port:9001

let do_build client package_name =
  let start = Time.Instant.now () in
  let build_succeeded = ref false in
  
  let callback = function
    | Tusk_client.BuildStarted _ ->
        Log.info "Build started..."
    | Tusk_client.BuildEvent event ->
        (* Print build progress events *)
        (match event.kind with
        | Tusk_model.Telemetry.Compiling { package } ->
            Log.info ("Compiling " ^ package)
        | _ -> ())
    | Tusk_client.BuildCompleted { stats; _ } ->
        let elapsed = Time.Instant.elapsed start in
        let elapsed_secs = Time.Duration.to_secs elapsed in
        Log.info (String.concat "" [
          "Finished in ";
          Float.to_string elapsed_secs;
          "s (";
          Int.to_string stats.built;
          " built, ";
          Int.to_string stats.failed;
          " failed, ";
          Int.to_string stats.skipped;
          " skipped)"
        ]);
        build_succeeded := true
    | Tusk_client.BuildFailed { stats; errors; _ } ->
        Log.error (String.concat "" [
          "Build failed: ";
          Int.to_string stats.failed;
          " error(s)"
        ]);
        List.iter (fun err ->
          Log.error ("  " ^ err.package)
        ) errors
    | Tusk_client.PlanningFailed { reason; _ } ->
        Log.error ("Planning failed: " ^ reason)
    | Tusk_client.CycleDetected { cycle_nodes; _ } ->
        Log.error ("Cycle detected: " ^ String.concat " -> " cycle_nodes)
  in
  
  match Tusk_client.build_streaming client (BuildPackage package_name) callback with
  | Ok _ -> if !build_succeeded then Ok () else Error (Failure "Build failed")
  | Error (Tusk_client.JsonrpcError err) ->
      Log.error ("RPC error: " ^ Tusk_client.jsonrpc_error_to_string err);
      Error (Failure "RPC error")
  | Error (Tusk_client.PackageNotFound { package_name; available_packages }) ->
      Log.error ("Package not found: " ^ package_name);
      Log.error ("Available: " ^ String.concat ", " available_packages);
      Error (Failure "Package not found")
  | Error (Tusk_client.UnexpectedEvent { reason; _ }) ->
      Log.error ("Unexpected event: " ^ reason);
      Error (Failure "Unexpected event")

let handle_rebuild client config process_ref =
  Log.info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━";
  Log.info "Rebuilding...";
  
  match do_build client config.package_name with
  | Ok () ->
      Log.info "Restarting...";
      
      (* Shutdown old process *)
      (match !process_ref with
      | Some p -> Process_manager.graceful_shutdown p
      | None -> ());
      
      (* Start new process *)
      let new_process = Process_manager.spawn
        ~cmd:(Path.to_string config.binary_path)
        ~args:config.binary_args in
      process_ref := Some new_process;
      
      Log.info "Server restarted";
      Log.info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  | Error _ ->
      Log.error "Build failed - fix errors and save again";
      Log.info "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

let start config =
  Log.info "Suri Development Server";
  Log.info ("Package: " ^ config.package_name);
  Log.info "";
  
  (* Connect to tusk server *)
  match connect_to_tusk () with
  | Error msg ->
      Log.error ("Failed to connect to tusk server: " ^ msg);
      Log.error "Make sure tusk server is running";
      Error (Failure "Connection failed")
  | Ok client ->
      (* Initial build *)
      Log.info "Building...";
      (match do_build client config.package_name with
      | Error _ ->
          Log.error "Initial build failed";
          Tusk_client.close client;
          Error (Failure "Build failed")
      | Ok () ->
          Log.info "Build complete";
          
          (* Start initial process *)
          let process = Process_manager.spawn
            ~cmd:(Path.to_string config.binary_path)
            ~args:config.binary_args in
          let process_ref = ref (Some process) in
          
          Log.info "";
          Log.info "Watching for changes... (Ctrl+C to stop)";
          Log.info "";
          
          (* Setup watcher and debouncer *)
          let watcher = File_watcher.create ~paths:config.watch_paths in
          let debouncer = Debouncer.create
            ~wait:(Time.Duration.from_millis 100)
            (fun _events -> handle_rebuild client config process_ref) in
          
          (* Main loop *)
          let rec loop () =
            (* Check for file changes *)
            (match File_watcher.next_event watcher ~timeout:(Time.Duration.from_secs 1) with
            | Some event -> Debouncer.push debouncer event
            | None -> ());
            
            (* Flush debouncer if needed *)
            Debouncer.tick debouncer;
            
            (* Check if process crashed *)
            (match !process_ref with
            | Some p ->
                (match Process_manager.status p with
                | Running -> ()
                | Exited 0 ->
                    Log.info "Process exited normally";
                    Tusk_client.close client;
                    exit 0
                | Exited code ->
                    let code_str = Int.to_string code in
                    Log.error ("Process crashed with code " ^ code_str);
                    Tusk_client.close client;
                    exit 1
                | Signaled signal ->
                    let signal_str = Int.to_string signal in
                    Log.error ("Process killed by signal " ^ signal_str);
                    Tusk_client.close client;
                    exit 1)
            | None -> ());
            
            loop ()
          in
          loop ();
          Ok ())
