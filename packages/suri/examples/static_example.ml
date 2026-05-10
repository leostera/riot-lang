open Std
open Suri

(**
   Static Files Middleware Example

   Demonstrates serving static files with security, caching, and directory browsing.

   Static files are located in: packages/suri/examples/public/

   Run with: riot run suri:static_example
   Then visit: http://localhost:8080/public/
*)

(** API routes (dynamic content) *)
let api_status = fun conn _req ->
  let json =
    Data.Json.(Object [
      ("status", String "ok");
      ("message", String "API is running");
      ("static_middleware", String "enabled");
    ])
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:(Data.Json.to_string json)
  |> Conn.with_header "content-type" "application/json"
  |> Conn.send

let api_info = fun conn _req ->
  let json =
    Data.Json.(Object [
      ("name", String "Static Files Example");
      (
        "features",
        Array [
          String "Security (path traversal, dotfiles)";
          String "Caching (ETag, Last-Modified, 304)";
          String "MIME type detection";
          String "Directory browsing";
        ]
      );
      (
        "endpoints",
        Array [ String "GET /api/status"; String "GET /api/info"; String "GET /public/*" ]
      );
    ])
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:(Data.Json.to_string json)
  |> Conn.with_header "content-type" "application/json"
  |> Conn.send

(** Root redirect *)
let root = fun conn _req ->
  conn
  |> Conn.respond ~status:Net.Http.Status.MovedPermanently ~body:""
  |> Conn.with_header "location" "/public/"
  |> Conn.send

(** API routes *)
let routes =
  Middleware.Router.[ get "/" root; get "/api/status" api_status; get "/api/info" api_info ]

(** Application with static files middleware *)
let app =
  Middleware.[
    request_id;
    logger;
    static ~at:"/public" (Path.v "./packages/suri/examples/public") ();
    static
      ~at:"/browse"
      ~config:{ Static.default_config with show_directory = true }
      (Path.v "./")
      ();
    router routes;
  ]

let main ~args:_ =
  Log.(set_level Debug);
  let port = 8_080 in
  match Suri.config ~port () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      Log.info "===========================================";
      Log.info "🎉 Static Files Middleware Example";
      Log.info "===========================================";
      Log.info (String.concat "" [ "Server: http://localhost:"; string_of_int port; "/" ]);
      Log.info "";
      Log.info "📂 Endpoints:";
      Log.info "  http://localhost:8080/              → Redirects to /public/";
      Log.info "  http://localhost:8080/public/       → index.html (auto)";
      Log.info "  http://localhost:8080/public/about.html";
      Log.info "  http://localhost:8080/public/assets/style.css → CSS with MIME type";
      Log.info "  http://localhost:8080/public/assets/app.js   → JavaScript";
      Log.info "  http://localhost:8080/browse/       → Directory listing";
      Log.info "";
      Log.info "🔒 Security Tests:";
      Log.info "  http://localhost:8080/public/../../../etc/passwd → 403 Forbidden";
      Log.info "  http://localhost:8080/public/.env.example → 403 Forbidden (dotfile)";
      Log.info "";
      Log.info "💾 Caching:";
      Log.info "  - First visit: 200 OK with ETag header";
      Log.info "  - Refresh: 304 Not Modified (cached!)";
      Log.info "  - Check Network tab in DevTools";
      Log.info "";
      Log.info "🌐 API Endpoints:";
      Log.info "  http://localhost:8080/api/status";
      Log.info "  http://localhost:8080/api/info";
      Log.info "";
      Log.info "===========================================";
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 8080";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
