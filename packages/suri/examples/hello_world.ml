open Std
open Suri

(**
   Simple Hello World Example

   Demonstrates the minimal Suri API - just a list of middleware!

   Run: riot run suri:hello_world
   Test: curl http://localhost:4000
*)
let app = [
  fun ~conn ~next:_ -> Conn.respond conn ~status:Ok ~body:"Hello from Suri!";
]

let main ~args:_ =
  Std.Config.load_file (Path.v "packages/suri/examples/conf.toml");
  let _ = Std.Log.start_link () in
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
  | Error _ ->
      Log.error "Failed to bind to port 4000";
      Error (Failure "Failed to start server")

let () = Runtime.run ~main ~args:Env.args ()
