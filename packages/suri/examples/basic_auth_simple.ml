open Std
open Suri

(** Simple Basic Auth demo - protecting admin routes *)
let home_handler = fun conn _req ->
  let html =
    String.concat
      ""
      [
        "<html><head><title>Basic Auth Demo</title></head><body>";
        "<h1>Basic Auth Example</h1>";
        "<p>This is a public page, no authentication required.</p>";
        "<h2>Try these protected routes:</h2>";
        "<ul>";
        "<li><a href=\"/admin\">Admin Panel</a> (user: admin, pass: secret)</li>";
        "<li><a href=\"/api/users\">API Endpoint</a> (user: api, pass: key123)</li>";
        "</ul>";
        "<p><strong>Note:</strong> Your browser will prompt for credentials when accessing protected routes.</p>";
        "</body></html>";
      ]
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let admin_handler = fun conn _req ->
  let html =
    String.concat
      ""
      [
        "<html><head><title>Admin Panel</title></head><body>";
        "<h1>Admin Panel</h1>";
        "<p style=\"color: green;\">✓ You are authenticated as <strong>admin</strong>!</p>";
        "<p>This page is protected by HTTP Basic Authentication.</p>";
        "<p><a href=\"/\">Back to home</a></p>";
        "</body></html>";
      ]
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let api_handler = fun conn _req ->
  let json = {|{"message": "Authenticated API access", "user": "api", "status": "ok"}|} in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
  |> Conn.with_header "content-type" "application/json"
  |> Conn.send

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/admin" admin_handler;
    get "/api/users" api_handler;
  ]

let main ~args:_ =
  let app = Middleware.[
    request_id;
    logger;
    basic_auth
      ~username:"admin"
      ~password:"secret"
      ~realm:"Protected Area"
      ~skip:(fun conn ->
        let path = Conn.path conn in
        String.equal path "/")
      ();
    router routes;
  ]
  in
  match Suri.config ~port:3_000 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          Log.info "===========================================";
          Log.info "Basic Auth Example Server Running";
          Log.info "===========================================";
          Log.info "Server: http://localhost:3000";
          Log.info "";
          Log.info "Routes:";
          Log.info "  /        - Public (no auth)";
          Log.info "  /admin   - Protected (admin/secret)";
          Log.info "  /api/users - Protected (admin/secret)";
          Log.info "";
          Log.info "Test with curl:";
          Log.info "  curl http://localhost:3000/";
          Log.info "  curl -u admin:secret http://localhost:3000/admin";
          Log.info "  curl -u admin:wrong http://localhost:3000/admin  # Should fail";
          Log.info "===========================================";
          (* Keep alive *)
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 3000";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
