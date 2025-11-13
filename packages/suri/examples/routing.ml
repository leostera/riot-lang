open Std
open Suri

(* Route handlers *)
let home_handler conn =
  let html = {|
<!DOCTYPE html>
<html>
  <head><title>Suri Example</title></head>
  <body>
    <h1>Welcome to Suri!</h1>
    <p>A supervised web framework for OCaml</p>
    <ul>
      <li><a href="/about">About</a></li>
      <li><a href="/api/health">Health Check (JSON)</a></li>
    </ul>
  </body>
</html>
  |} in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "text/html"
  |> Middleware.Conn.with_body html
  |> Middleware.Conn.send

let about_handler conn =
  conn
  |> Middleware.Conn.respond ~status:Ok 
      ~body:"Suri - High-performance web framework with OTP-style supervision"
  |> Middleware.Conn.send

let health_handler conn =
  let json = Data.Json.obj [
    ("status", Data.Json.string "ok");
    ("service", Data.Json.string "suri");
  ] in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string json)
  |> Middleware.Conn.send

let not_found_handler conn =
  conn
  |> Middleware.Conn.respond ~status:NotFound ~body:"404 - Page not found"
  |> Middleware.Conn.send

(* Request logger middleware *)
let logger_middleware conn =
  let method_ = Middleware.Conn.method_ conn in
  let uri = Middleware.Conn.uri conn in
  Log.info ((Net.Http.Method.to_string method_) ^ " " ^ uri);
  conn

(* Define routes *)
let routes = Middleware.Router.[
  get "/" home_handler;
  get "/about" about_handler;
  get "/api/health" health_handler;
]

(* Build middleware pipeline *)
let app = Middleware.Pipeline.[
  logger_middleware;
  Middleware.Router.middleware routes;
  not_found_handler;  (* 404 fallback *)
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
      
      Log.info "Server with routing on http://0.0.0.0:3000";
      Log.info "Routes:";
      Log.info "  GET  /           - Home page";
      Log.info "  GET  /about      - About page";
      Log.info "  GET  /api/health - Health check";
      
      (* Monitor supervisor *)
      let count = Supervisor.Dynamic.count_children supervisor in
      Log.info ("Running with " ^ (Int.to_string count.active) ^ " acceptors under supervision");
      
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
