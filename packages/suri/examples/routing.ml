open Std
open Suri

(** Routing Example
    
    Demonstrates routing with middleware pipeline.
    
    Run: tusk run suri:routing *)

(* Custom middleware *)
let logger_middleware conn =
  let method_ = Conn.method_ conn in
  let uri = Conn.uri conn in
  Log.info ((Net.Http.Method.to_string method_) ^ " " ^ uri);
  conn

(* Route handlers *)
let home_handler conn =
  let html = {|
<!DOCTYPE html>
<html>
  <head><title>Suri Example</title></head>
  <body>
    <h1>Welcome to Suri!</h1>
    <ul>
      <li><a href="/about">About</a></li>
      <li><a href="/api/health">Health Check</a></li>
    </ul>
  </body>
</html>
  |} in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "text/html"
  |> Conn.with_body html
  |> Conn.send

let about_handler conn =
  conn
  |> Conn.respond ~status:Ok 
      ~body:"Suri - High-performance web framework"
  |> Conn.send

let health_handler conn =
  let json = Data.Json.obj [
    ("status", Data.Json.string "ok");
    ("service", Data.Json.string "suri");
  ] in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string json)
  |> Conn.send

let not_found_handler conn =
  conn
  |> Conn.respond ~status:NotFound ~body:"404 - Page not found"
  |> Conn.send

(* Define routes *)
let routes = Middleware.Router.[
  get "/" home_handler;
  get "/about" about_handler;
  get "/api/health" health_handler;
]

(* App is just a list of middleware! *)
let app = [
  logger_middleware;
  Middleware.router routes;
  not_found_handler;  (* 404 fallback *)
]

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    match Suri.start_link app with
    | Ok supervisor ->
        Log.info "🚀 Server with routing on http://0.0.0.0:4000";
        Log.info "   Routes:";
        Log.info "     GET  /           - Home page";
        Log.info "     GET  /about      - About page";
        Log.info "     GET  /api/health - Health check";
        
        let count = Supervisor.Dynamic.count_children supervisor in
        Log.info ("   Running with " ^ Int.to_string count.active ^ " acceptors");
        
        let rec loop () =
          sleep (Time.Duration.from_secs 100);
          loop ()
        in
        loop ()
    | Error `Bind_error ->
        Log.error "Failed to bind to port 4000";
        Error (Failure "Failed to start server")
  )
