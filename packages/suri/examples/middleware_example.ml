open Std
open Suri

(* Custom middleware: Add CORS headers *)
let cors_middleware conn =
  conn
  |> Middleware.Conn.with_header "Access-Control-Allow-Origin" "*"
  |> Middleware.Conn.with_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE"
  |> Middleware.Conn.with_header "Access-Control-Allow-Headers" "Content-Type"

(* Custom middleware: Request logger *)
let logger_middleware conn =
  let method_ = Middleware.Conn.method_ conn in
  let uri = Middleware.Conn.uri conn in
  Log.info ((Net.Http.Method.to_string method_) ^ " " ^ uri);
  conn

(* Custom middleware: Add request ID *)
let request_id_middleware =
  let counter = ref 0 in
  fun conn ->
    let id = !counter in
    counter := id + 1;
    Middleware.Conn.with_header "X-Request-ID" (Int.to_string id) conn

(* Route handlers *)
let home_handler conn =
  let html = {|
<!DOCTYPE html>
<html>
  <head><title>Middleware Example</title></head>
  <body>
    <h1>Suri Middleware Example</h1>
    <p>This response went through multiple middleware layers:</p>
    <ul>
      <li>Request logger</li>
      <li>CORS headers</li>
      <li>Request ID assignment</li>
      <li>Router</li>
    </ul>
    <p>Check the response headers to see X-Request-ID!</p>
    <nav>
      <a href="/api/data">API Data</a> |
      <a href="/about">About</a>
    </nav>
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

let api_data_handler conn =
  let data = Data.Json.obj [
    ("message", Data.Json.string "Hello from API");
    ("middleware_count", Data.Json.int 4);
    ("features", Data.Json.array [
      Data.Json.string "CORS";
      Data.Json.string "Logging";
      Data.Json.string "Request IDs";
      Data.Json.string "Routing";
    ])
  ] in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string data)
  |> Middleware.Conn.send

let not_found_handler conn =
  conn
  |> Middleware.Conn.respond ~status:NotFound ~body:"404 - Not Found"
  |> Middleware.Conn.send

(* Define routes *)
let routes = Middleware.Router.[
  get "/" home_handler;
  get "/about" about_handler;
  get "/api/data" api_data_handler;
]

(* Build middleware pipeline *)
let app = Middleware.Pipeline.[
  logger_middleware;        (* Log all requests *)
  request_id_middleware;    (* Assign request IDs *)
  cors_middleware;          (* Add CORS headers *)
  Middleware.Router.middleware routes;  (* Route to handlers *)
  not_found_handler;        (* Fallback 404 *)
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
        | Error `Bind_error -> panic "Failed to bind to port"
      in
      
      Log.info "Middleware example server on http://0.0.0.0:3000";
      Log.info "Middleware stack:";
      Log.info "  1. Logger - logs all requests";
      Log.info "  2. Request ID - assigns unique IDs";
      Log.info "  3. CORS - adds cross-origin headers";
      Log.info "  4. Router - matches routes";
      Log.info "  5. 404 fallback";
      Log.info "";
      Log.info "Try:";
      Log.info "  curl -v http://localhost:3000/";
      Log.info "  curl http://localhost:3000/api/data";
      
      let count = Supervisor.Dynamic.count_children supervisor in
      Log.info ((Int.to_string count.active) ^ " acceptors ready");
      
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
