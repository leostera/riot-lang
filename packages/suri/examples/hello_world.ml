open Std
open Suri

(* Simple handler using middleware abstractions *)
let hello_handler conn =
  conn
  |> Middleware.Conn.respond ~status:Ok ~body:"Hello from Suri!"
  |> Middleware.Conn.send

(* Define routes *)
let routes = Middleware.Router.[
  get "/" hello_handler;
]

(* Build simple middleware pipeline *)
let app = Middleware.Pipeline.[
  Middleware.Router.middleware routes;
]

(* WebServer handler that runs the middleware pipeline *)
let handler socket_conn req =
  let conn = Middleware.Conn.make socket_conn req in
  let conn = Middleware.Pipeline.run conn app in
  let response = Middleware.Conn.to_response conn in
  WebServer.Handler.close response

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    (* Start the server in its own process *)
    let _server_pid = spawn (fun () ->
      let config = WebServer.Config.make () in
      let supervisor = match WebServer.start_link ~port:3000 ~config ~handler () with
        | Ok s -> s
        | Error `Bind_error ->
            Log.error "Failed to bind to port 3000";
            panic "Failed to start server"
      in
      
      Log.info "Server running on http://0.0.0.0:3000";
      Log.info "Try: curl http://localhost:3000";
      
      (* Monitor the supervisor *)
      let count = Supervisor.Dynamic.count_children supervisor in
      Log.info ("Started with " ^ (Int.to_string count.active) ^ " acceptors");
      
      (* Server process waits forever *)
      let rec loop () =
        let _ = receive_any () in
        loop ()
      in
      loop ()
    ) in
    
    (* Wait forever *)
    let rec loop () =
      let _ = receive_any () in
      loop ()
    in
    loop ()
  )
