open Std
open Suri

(** Basic Auth with custom validation - simulating database lookup *)

(** Mock database module *)
module DB = struct
  type user = { id: int; username: string; password_hash: string; role: string; email: string }

  (* Simulated user database *)

  let users = [
    {
      id = 1;
      username = "alice";
      password_hash = "hash_of_pass1";
      role = "admin";
      email = "alice@example.com";
    };
    {
      id = 2;
      username = "bob";
      password_hash = "hash_of_pass2";
      role = "user";
      email = "bob@example.com";
    };
    {
      id = 3;
      username = "charlie";
      password_hash = "hash_of_pass3";
      role = "user";
      email = "charlie@example.com";
    };
  ]

  let find_user = fun username -> List.find users ~fn:(fun u -> String.equal u.username username)

  let verify_password = fun user password ->
    (* In real app: use bcrypt, argon2, etc. *)
    (* For demo: just check if hash matches "hash_of_" + password *)
    String.equal
      user.password_hash
      (String.concat "" [ "hash_of_"; password ])
end

(** Custom validation function *)
let validate = fun ~username ~password ->
  match DB.find_user username with
  | Some user when DB.verify_password user password ->
      Log.info (String.concat "" [ "User authenticated: "; username; " ("; user.role; ")"; ]);
      Some user
  | Some _ ->
      Log.warn (String.concat "" [ "Failed auth attempt for: "; username; " (wrong password)" ]);
      None
  | None ->
      Log.warn (String.concat "" [ "Failed auth attempt for unknown user: "; username ]);
      None

let user_key: DB.user Middleware.Basic_auth.key = Middleware.Basic_auth.key ()

(** Route handlers *)
let home_handler = fun conn _req ->
  let html =
    String.concat
      ""
      [
        "<html><head><title>Database Auth Demo</title></head><body>";
        "<h1>Basic Auth - Database Validation Example</h1>";
        "<p>This example validates credentials against a mock database.</p>";
        "<h2>Test Accounts:</h2>";
        "<table border=\"1\" cellpadding=\"10\">";
        "<tr><th>Username</th><th>Password</th><th>Role</th></tr>";
        "<tr><td>alice</td><td>pass1</td><td>admin</td></tr>";
        "<tr><td>bob</td><td>pass2</td><td>user</td></tr>";
        "<tr><td>charlie</td><td>pass3</td><td>user</td></tr>";
        "</table>";
        "<h2>Protected Routes:</h2>";
        "<ul>";
        "<li><a href=\"/dashboard\">Dashboard</a> - Shows your user info</li>";
        "<li><a href=\"/admin\">Admin Panel</a> - Admins only</li>";
        "<li><a href=\"/profile\">Profile</a> - Shows your profile</li>";
        "</ul>";
        "<h3>Test with curl:</h3>";
        "<pre>";
        "curl -u alice:pass1 http://localhost:3003/dashboard\n";
        "curl -u bob:pass2 http://localhost:3003/profile\n";
        "curl -u charlie:wrong http://localhost:3003/dashboard  # Should fail\n";
        "</pre>";
        "</body></html>";
      ]
  in
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let dashboard_handler = fun conn _req ->
  (* Get authenticated user from connection *)
  match Middleware.Basic_auth.get user_key conn with
  | Some user ->
      let html =
        String.concat
          ""
          [
            "<html><head><title>Dashboard</title></head><body>";
            "<h1>Dashboard</h1>";
            "<p style=\"color: green;\">✓ Authenticated!</p>";
            "<div style=\"background: #f5f5f5; padding: 15px; border-radius: 5px; margin: 20px 0;\">";
            "<h3>User Info</h3>";
            "<p><strong>ID:</strong> ";
            string_of_int user.DB.id;
            "</p>";
            "<p><strong>Username:</strong> ";
            user.DB.username;
            "</p>";
            "<p><strong>Email:</strong> ";
            user.DB.email;
            "</p>";
            "<p><strong>Role:</strong> ";
            user.DB.role;
            "</p>";
            "</div>";
            "<p><a href=\"/\">Back to home</a></p>";
            "</body></html>";
          ]
      in
      conn
      |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
      |> Conn.with_header "content-type" "text/html"
      |> Conn.send
  | None ->
      (* Should never happen if middleware is configured correctly *)
      conn
      |> Conn.respond ~status:Net.Http.Status.Unauthorized ~body:"Unauthorized"
      |> Conn.send

let admin_handler = fun conn _req ->
  match Middleware.Basic_auth.get user_key conn with
  | Some user ->
      (* Check if user is admin *)
      if String.equal user.DB.role "admin" then
        let html =
          String.concat
            ""
            [
              "<html><head><title>Admin Panel</title></head><body>";
              "<h1>Admin Panel</h1>";
              "<p style=\"color: green;\">✓ Admin access granted!</p>";
              "<p>Welcome, <strong>";
              user.DB.username;
              "</strong></p>";
              "<p>This page is only accessible to administrators.</p>";
              "<p><a href=\"/\">Back to home</a></p>";
              "</body></html>";
            ]
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
        |> Conn.with_header "content-type" "text/html"
        |> Conn.send
      else
        let html =
          String.concat
            ""
            [
              "<html><head><title>Forbidden</title></head><body>";
              "<h1>403 Forbidden</h1>";
              "<p style=\"color: red;\">You do not have permission to access this page.</p>";
              "<p>Your role: <strong>";
              user.DB.role;
              "</strong></p>";
              "<p>Required role: <strong>admin</strong></p>";
              "<p><a href=\"/\">Back to home</a></p>";
              "</body></html>";
            ]
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:html
        |> Conn.with_header "content-type" "text/html"
        |> Conn.send
  | None ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Unauthorized ~body:"Unauthorized"
      |> Conn.send

let profile_handler = fun conn _req ->
  match Middleware.Basic_auth.get user_key conn with
  | Some user ->
      let json =
        String.concat
          ""
          [
            "{\"id\": ";
            string_of_int user.DB.id;
            ", \"username\": \"";
            user.DB.username;
            "\"";
            ", \"email\": \"";
            user.DB.email;
            "\"";
            ", \"role\": \"";
            user.DB.role;
            "\"";
            "}";
          ]
      in
      conn
      |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
      |> Conn.with_header "content-type" "application/json"
      |> Conn.send
  | None ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Unauthorized ~body:"{\"error\": \"Unauthorized\"}"
      |> Conn.with_header "content-type" "application/json"
      |> Conn.send

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/dashboard" dashboard_handler;
    get "/admin" admin_handler;
    get "/profile" profile_handler;
  ]

let main ~args:_ =
  let app = Middleware.[
    request_id;
    logger;
    basic_auth_with_validation
      ~validate
      ~assign_to:user_key
      ~realm:"Member Area"
      ~skip:(fun conn ->
        let path = Conn.path conn in
        (* Only skip the home page *)
        String.equal path "/")
      ();
    router routes;
  ]
  in
  match Suri.config ~port:3_003 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          Log.info "===========================================";
          Log.info "Database Validation Basic Auth Example";
          Log.info "===========================================";
          Log.info "Server: http://localhost:3003";
          Log.info "";
          Log.info "Test Accounts:";
          Log.info "  alice:pass1 (admin)";
          Log.info "  bob:pass2 (user)";
          Log.info "  charlie:pass3 (user)";
          Log.info "";
          Log.info "Routes:";
          Log.info "  /           - Public";
          Log.info "  /dashboard  - All authenticated users";
          Log.info "  /admin      - Admin role only";
          Log.info "  /profile    - All authenticated users (JSON)";
          Log.info "";
          Log.info "Test commands:";
          Log.info "  curl http://localhost:3003/";
          Log.info "  curl -u alice:pass1 http://localhost:3003/dashboard";
          Log.info "  curl -u alice:pass1 http://localhost:3003/admin  # Admin access";
          Log.info "  curl -u bob:pass2 http://localhost:3003/admin    # Forbidden (not admin)";
          Log.info "  curl -u bob:pass2 http://localhost:3003/profile";
          Log.info "  curl -u charlie:pass3 http://localhost:3003/dashboard";
          Log.info "";
          Log.info "Check the logs to see authentication attempts!";
          Log.info "===========================================";
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 3003";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
