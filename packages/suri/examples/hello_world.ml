open Std
open Suri
(** Simple Hello World Example
    
    Demonstrates the minimal Suri API - just a list of middleware!
    
    Run: tusk run suri:hello_world
    Test: curl http://localhost:4000 *)
let app = [ (fun ~conn ~next:_ -> Conn.respond conn ~status:Ok ~body:"Hello from Suri!") ]

let () =
  Miniriot.run ~args:Env.args ()
    ~main:(fun ~args:_ ->
      match Suri.start_link app with
      | Ok supervisor ->
          Log.info "🚀 Server running on http://0.0.0.0:4000";
          Log.info "   Try: curl http://localhost:4000";
          let count = Supervisor.Dynamic.count_children supervisor in
          Log.info ("   Started with " ^ Int.to_string count.active ^ " acceptors");
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error `Bind_error ->
          Log.error "Failed to bind to port 4000";
          Error (Failure "Failed to start server"))
