open Std
open Suri

(* Custom middleware: Add CORS headers *)

let cors_middleware = fun ~conn ~next ->
    let conn' = next conn in
    conn'
    |> Conn.with_header "Access-Control-Allow-Origin" "*"
    |> Conn.with_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE"
    |> Conn.with_header "Access-Control-Allow-Headers" "Content-Type"

(* Custom middleware: Timing *)

let timer_middleware = fun ~conn ~next ->
    let start = Time.Instant.now () in
    let conn' = next conn in
    let duration = Time.Instant.elapsed start |> Time.Duration.to_millis in
    Log.debug (String.concat "" [ "Request took "; Int.to_string duration; "ms" ]);
    conn'

(* Route handlers *)

let home_handler = fun conn _req ->
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
  |}
    in
    conn
    |> Conn.with_status Ok
    |> Conn.with_header "Content-Type" "text/html"
    |> Conn.with_body html
    |> Conn.send

let about_handler = fun conn _req ->
    conn
    |> Conn.respond ~status:Ok ~body:"Suri - High-performance web framework with OTP-style supervision"
    |> Conn.send

let api_data_handler = fun conn _req ->
    (* Get the request ID from the response headers (set by request_id middleware) *)
    let resp_headers = Conn.resp_headers conn in
    let request_id = List.assoc_opt "x-request-id" resp_headers |> Option.unwrap_or ~default:"unknown" in
    let data = Data.Json.obj
      [
        ("message", Data.Json.string "Hello from API");
        ("request_id", Data.Json.string request_id);
        ("middleware_count", Data.Json.int 5);
        (
          "features",
          Data.Json.array
            [
              Data.Json.string "Request IDs";
              Data.Json.string "CORS";
              Data.Json.string "Logging";
              Data.Json.string "Timing";
              Data.Json.string "Routing";

            ]
        )
      ] in
    conn
    |> Conn.with_status Ok
    |> Conn.with_header "Content-Type" "application/json"
    |> Conn.with_body (Data.Json.to_string data)
    |> Conn.send

let not_found_handler = fun conn _req ->
    conn |> Conn.respond ~status:NotFound ~body:"404 - Not Found" |> Conn.send

(* Define routes *)

let routes = Middleware.Router.[get "/" home_handler;
get "/about" about_handler;
get "/api/data" api_data_handler;]

(* Build middleware pipeline *)

(* Middleware wraps the next handler in the chain! *)

let app = Middleware.[ request_id; logger; timer_middleware; cors_middleware; router routes;  ]

let () =
  Miniriot.run ~args:Env.args ()
    ~main:(fun ~args:_ ->
      match Suri.start_link app with
      | Ok supervisor ->
          Log.info "🚀 Middleware example server on http://0.0.0.0:4000";
          Log.info "   Middleware stack:";
          Log.info "     1. Request ID - generates/preserves x-request-id";
          Log.info "     2. Logger - logs all requests";
          Log.info "     3. Timer - measures request duration";
          Log.info "     4. CORS - adds cross-origin headers";
          Log.info "     5. Router - matches routes";
          Log.info "";
          Log.info "   Try:";
          Log.info "     curl -v http://localhost:4000/";
          Log.info "     curl http://localhost:4000/api/data";
          Log.info "     curl -H \"x-request-id: my-custom-id\" http://localhost:4000/";
          let count = Supervisor.Dynamic.count_children supervisor in
          Log.info ("   " ^ Int.to_string count.active ^ " acceptors ready");
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error `Bind_error ->
          Log.error "Failed to bind to port 4000";
          Error (Failure "Failed to start server"))
