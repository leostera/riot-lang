open Std
open Suri

(** Routing Example
    
    Demonstrates routing with middleware pipeline including request ID and logging.
    
    Run: tusk run suri:routing *)
(* Route handlers *)

let home_handler = fun conn req ->
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
  |}
  in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "text/html"
  |> Conn.with_body html
  |> Conn.send

let about_handler = fun conn req ->
  conn |> Conn.respond ~status:Ok ~body:"Suri - High-performance web framework" |> Conn.send

let health_handler = fun conn req ->
  (* Get the request ID that was added by request_id middleware *)
  let resp_headers = Conn.resp_headers conn in
  let request_id = List.assoc_opt "x-request-id" resp_headers |> Option.unwrap_or ~default:"unknown" in
  let json = Data.Json.obj
    [
      ("status", Data.Json.string "ok");
      ("service", Data.Json.string "suri");
      ("request_id", Data.Json.string request_id);
    ] in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string json)
  |> Conn.send

let not_found_handler = fun conn req ->
  conn |> Conn.respond ~status:NotFound ~body:"404 - Page not found" |> Conn.send

(* Define routes *)

let routes = Middleware.Router.[get "/" home_handler;
get "/about" about_handler;
get "/api/health" health_handler;]

(* App with request ID tracking and logging! *)

let app = Middleware.[ request_id; logger; router routes; ]

let () =
  Miniriot.run ~args:Env.args ()
    ~main:(fun ~args:_ ->
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
          Error (Failure "Failed to start server"))
