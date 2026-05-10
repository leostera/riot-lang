open Std
open Suri

(** Simple API with CORS enabled for cross-origin requests *)
let routes =
  Middleware.Router.[
    get
      "/api/hello"
      (fun conn _req ->
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"Hello from CORS-enabled API!"
        |> Conn.send);
    get
      "/api/data"
      (fun conn _req ->
        let json = {|{"message": "This is accessible from browsers", "status": "ok"}|} in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
        |> Conn.with_header "content-type" "application/json"
        |> Conn.send);
    post
      "/api/submit"
      (fun conn _req ->
        let body = Conn.body conn in
        Log.info (String.concat "" [ "Received POST data: "; body ]);
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"Data received!"
        |> Conn.send);
  ]

let main ~args:_ =
  (* Development mode - allow all origins *)
  match Middleware.cors ~origins:[ "*" ] () with
  | Error error -> Error (Failure (Middleware.Cors.config_error_to_string error))
  | Ok cors_middleware ->
      let app = Middleware.[ request_id; logger; cors_middleware; router routes; ] in
      match Suri.config ~port:4_000 () with
      | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
      | Ok config ->
          match Suri.start_link ~config app with
          | Ok _supervisor ->
              Log.info "===========================================";
              Log.info "CORS Example Server Running";
              Log.info "===========================================";
              Log.info "Server: http://localhost:4000";
              Log.info "";
              Log.info "Try these endpoints:";
              Log.info "  GET  /api/hello";
              Log.info "  GET  /api/data";
              Log.info "  POST /api/submit";
              Log.info "";
              Log.info "Test with curl:";
              Log.info "  curl -H 'Origin: https://example.com' http://localhost:4000/api/hello";
              Log.info "";
              Log.info "Test preflight:";
              Log.info "  curl -X OPTIONS -H 'Origin: https://example.com' \\";
              Log.info "       -H 'Access-Control-Request-Method: POST' \\";
              Log.info "       http://localhost:4000/api/submit";
              Log.info "===========================================";
              (* Keep alive *)
              let rec loop () =
                sleep (Time.Duration.from_secs 100);
                loop ()
              in
              loop ()
          | Error error ->
              Log.error "Failed to bind to port 4000";
              Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
