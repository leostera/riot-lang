open Std

type Message.t += Ping
let () =
  Miniriot.run ~main:(fun ~args:_ ->
    Std.Log.(set_level Debug);
    eprintln "🔍 Simple File Watcher Test\n";
    
    (* Get current directory *)
    let workspace_root = 
      Env.current_dir () 
      |> Result.expect ~msg:"Failed to get cwd" 
    in
    
    (* Configure file watcher *)
    (* Start watcher *)
    eprintln "Starting file watcher...";
    let _watcher_pid : Pid.t = Fs.FileWatcher.start_link ~root:workspace_root () in
    
    eprintln "✅ File watcher started!";
    eprintln "Press Ctrl+C to stop\n";


        (* Simple event loop *)
    let rec loop () =
      let selector = function
        | Fs.FileWatcher.FileEvents events -> `select events
        | _ -> `skip
      in
      
      match receive ~selector () with
      | events ->
        List.iter (fun ev -> println (Data.Json.to_string (Fs.Event.to_json ev))) events;
        loop ()
      | exception exn ->
          println ("Error: " ^ Exception.to_string exn);
          loop ()
    in
    
    loop ()
  ) ~args:[] ()
