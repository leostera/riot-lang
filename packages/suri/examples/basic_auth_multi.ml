open Std
open Suri

(** Basic Auth with different credentials for different routes *)
let home_handler = fun conn _req ->
  let html =
    String.concat
      ""
      [
        "<html><head><title>Multi-Credential Basic Auth</title></head><body>";
        "<h1>Basic Auth - Multiple Credentials Example</h1>";
        "<p>This example shows different credentials for different routes.</p>";
        "<h2>Try these protected routes:</h2>";
        "<ul>";
        "<li><a href=\"/admin\">Admin Panel</a> - Credentials: <code>admin:secret</code></li>";
        "<li><a href=\"/api/users\">API Users Endpoint</a> - Credentials: <code>api:key123</code></li>";
        "<li><a href=\"/api/posts\">API Posts Endpoint</a> - Credentials: <code>api:key123</code></li>";
        "</ul>";
        "<p><strong>Note:</strong> Each section requires different credentials!</p>";
        "<h3>Test with curl:</h3>";
        "<pre>";
        "curl -u admin:secret http://localhost:3002/admin\n";
        "curl -u api:key123 http://localhost:3002/api/users\n";
        "curl -u admin:secret http://localhost:3002/api/users  # Should fail - wrong creds\n";
        "</pre>";
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
        "<p style=\"color: green;\">✓ Authenticated as <strong>admin</strong></p>";
        "<p>This route requires credentials: <code>admin:secret</code></p>";
        "<p><a href=\"/\">Back to home</a></p>";
        "</body></html>";
      ]
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let api_users_handler = fun conn _req ->
  let json =
    {|{"endpoint": "users", "message": "API authenticated", "user": "api", "data": ["user1", "user2", "user3"]}|}
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
  |> Conn.with_header "content-type" "application/json"
  |> Conn.send

let api_posts_handler = fun conn _req ->
  let json =
    {|{"endpoint": "posts", "message": "API authenticated", "user": "api", "data": ["post1", "post2"]}|}
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
  |> Conn.with_header "content-type" "application/json"
  |> Conn.send

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/admin" admin_handler;
    get "/api/users" api_users_handler;
    get "/api/posts" api_posts_handler;
  ]

let main ~args:_ =
  (* Strategy: Use skip functions to apply different auth to different paths *)
  let app = Middleware.[
    request_id;
    logger;
    basic_auth
      ~username:"admin"
      ~password:"secret"
      ~realm:"Admin Area"
      ~skip:(fun conn ->
        let path = Conn.path conn in
        (* Skip everything except /admin *)
        not (String.equal path "/admin"))
      ();
    basic_auth
      ~username:"api"
      ~password:"key123"
      ~realm:"API Access"
      ~skip:(fun conn ->
        let path = Conn.path conn in
        (* Skip everything except /api/* *)
        not (String.starts_with path ~prefix:"/api"))
      ();
    router routes;
  ]
  in
  match Suri.config ~port:3_002 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          Log.info "===========================================";
          Log.info "Multi-Credential Basic Auth Example";
          Log.info "===========================================";
          Log.info "Server: http://localhost:3002";
          Log.info "";
          Log.info "Routes and Credentials:";
          Log.info "  /          - Public (no auth)";
          Log.info "  /admin     - admin:secret";
          Log.info "  /api/users - api:key123";
          Log.info "  /api/posts - api:key123";
          Log.info "";
          Log.info "Test commands:";
          Log.info "  curl http://localhost:3002/";
          Log.info "  curl -u admin:secret http://localhost:3002/admin";
          Log.info "  curl -u api:key123 http://localhost:3002/api/users";
          Log.info "  curl -u api:key123 http://localhost:3002/api/posts";
          Log.info "";
          Log.info "These should fail (wrong credentials):";
          Log.info "  curl -u api:key123 http://localhost:3002/admin";
          Log.info "  curl -u admin:secret http://localhost:3002/api/users";
          Log.info "===========================================";
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 3002";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
