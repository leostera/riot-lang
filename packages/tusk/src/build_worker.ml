(** Build worker process - handles building individual packages *)
open Miniriot
open Build_messages

(** Worker loop that processes build tasks *)
let worker_loop server_pid worker_id =
  let rec loop () =
    (* Request next task from the server *)
    send server_pid (NextTask (self ()));
    
    match receive () with
    | Task pkg_name ->
        (* Build the package *)
        Printf.printf "[Worker %d] Would build package: %s\n" worker_id pkg_name;
        flush stdout;
        
        (* Simulate some build time *)
        sleep 0.1;
        
        (* Send result back to server *)
        send server_pid (TaskComplete (pkg_name, true));
        
        (* Continue working *)
        loop ()
    
    | NoTask ->
        (* No tasks available, wait a bit and try again *)
        sleep 0.5;
        loop ()
        
    | Shutdown ->
        Printf.printf "[Worker %d] Shutting down\n" worker_id;
        Process.Normal
        
    | _ ->
        (* Ignore other messages *)
        loop ()
  in
  loop ()

(** Main entry point for worker process *)
let main server_pid worker_id () =
  Printf.printf "[Worker %d] Started (pid: %s)\n" worker_id (Pid.to_string (self ()));
  flush stdout;
  worker_loop server_pid worker_id