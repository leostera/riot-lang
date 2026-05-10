open Std
open Suri

(** Example app demonstrating body_parser middleware with CSRF protection *)

(** Home page with form *)
let home = fun conn _req ->
  match Middleware.Csrf.hidden_field conn with
  | Error error ->
      conn
      |> Conn.with_status Net.Http.Status.InternalServerError
      |> Conn.with_body (Middleware.Csrf.error_to_string error)
      |> Conn.send
  | Ok csrf_field ->
      let html =
        {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Body Parser Example</title>
  <style>
    body { font-family: system-ui; max-width: 800px; margin: 40px auto; padding: 20px; }
    form { background: #f5f5f5; padding: 20px; border-radius: 8px; margin: 20px 0; }
    label { display: block; margin: 10px 0 5px; font-weight: 500; }
    input, textarea { width: 100%; padding: 8px; border: 1px solid #ddd; border-radius: 4px; box-sizing: border-box; }
    button { background: #0066cc; color: white; padding: 10px 20px; border: none; border-radius: 4px; cursor: pointer; margin-top: 10px; }
    button:hover { background: #0052a3; }
    .result { background: #e8f5e9; padding: 20px; border-radius: 8px; margin: 20px 0; }
    .json-example { background: #fff3e0; padding: 20px; border-radius: 8px; margin: 20px 0; }
    pre { background: #000; color: #0f0; padding: 10px; border-radius: 4px; overflow-x: auto; }
  </style>
</head>
<body>
  <h1>🔐 Body Parser + CSRF Example</h1>
  
  <h2>HTML Form (urlencoded)</h2>
  <form method="POST" action="/submit">
    |}
        ^ Component.to_html csrf_field
        ^ {|
    <label for="name">Name:</label>
    <input type="text" id="name" name="name" value="Alice" required>
    
    <label for="email">Email:</label>
    <input type="email" id="email" name="email" value="alice@example.com" required>
    
    <label for="message">Message:</label>
    <textarea id="message" name="message" rows="4">Hello from body parser!</textarea>
    
    <button type="submit">Submit Form</button>
  </form>
  
  <h2>JSON API Example</h2>
  <div class="json-example">
    <p>Try this with curl:</p>
    <pre>curl -X POST http://localhost:8080/api/data \
  -H "Content-Type: application/json" \
  -d '{"name": "Bob", "action": "test"}'</pre>
  </div>
  
  <h2>How It Works</h2>
  <ul>
    <li><strong>body_parser</strong> middleware parses form data and JSON automatically</li>
    <li>Parsed data is available in <code>Conn.body_params</code></li>
    <li><strong>CSRF</strong> middleware reads the token from <code>body_params</code></li>
    <li>No manual parsing required!</li>
  </ul>
</body>
</html>|}
      in
      conn
      |> Conn.with_status Net.Http.Status.Ok
      |> Conn.with_body html
      |> Conn.send

(** Handle form submission *)
let submit_form = fun conn _req ->
  let body_params = Conn.body_params conn in
  (* Extract form fields *)
  let name =
    Std.Collections.Proplist.get body_params ~key:"name"
    |> Option.unwrap_or ~default:"Unknown"
  in
  let email =
    Std.Collections.Proplist.get body_params ~key:"email"
    |> Option.unwrap_or ~default:"unknown@example.com"
  in
  let message =
    Std.Collections.Proplist.get body_params ~key:"message"
    |> Option.unwrap_or ~default:""
  in
  let html =
    {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>Form Submitted</title>
  <style>
    body { font-family: system-ui; max-width: 800px; margin: 40px auto; padding: 20px; }
    .success { background: #e8f5e9; padding: 20px; border-radius: 8px; border-left: 4px solid #4caf50; }
    .data { background: #f5f5f5; padding: 15px; border-radius: 4px; margin: 15px 0; }
    a { color: #0066cc; text-decoration: none; }
    a:hover { text-decoration: underline; }
  </style>
</head>
<body>
  <div class="success">
    <h1>✅ Form Submitted Successfully!</h1>
    <p>Your data was parsed by <strong>body_parser</strong> middleware and validated by <strong>CSRF</strong> protection.</p>
  </div>
  
  <h2>Received Data (from Conn.body_params):</h2>
  <div class="data">
    <p><strong>Name:</strong> |}
    ^ name
    ^ {|</p>
    <p><strong>Email:</strong> |}
    ^ email
    ^ {|</p>
    <p><strong>Message:</strong> |}
    ^ message
    ^ {|</p>
  </div>
  
  <p><a href="/">← Back to form</a></p>
</body>
</html>|}
  in
  conn
  |> Conn.with_status Net.Http.Status.Ok
  |> Conn.with_body html
  |> Conn.send

(** Handle JSON API request *)
let api_handler = fun conn _req ->
  let body_params = Conn.body_params conn in
  (* Extract JSON fields *)
  let name =
    Std.Collections.Proplist.get body_params ~key:"name"
    |> Option.unwrap_or ~default:"Unknown"
  in
  let action =
    Std.Collections.Proplist.get body_params ~key:"action"
    |> Option.unwrap_or ~default:"none"
  in
  (* Create JSON response *)
  let response_json =
    Data.Json.(Object [
      ("success", Bool true);
      ("message", String "Data received and parsed");
      ("received", Object [ ("name", String name); ("action", String action); ]);
      ("middleware", String "body_parser automatically parsed the JSON body");
    ])
  in
  let body = Data.Json.to_string response_json in
  conn
  |> Conn.with_status Net.Http.Status.Ok
  |> Conn.with_header "content-type" "application/json"
  |> Conn.with_body body
  |> Conn.send

(** Routes *)
let routes =
  Middleware.Router.[ get "/" home; post "/submit" submit_form; post "/api/data" api_handler ]

(** Application with body_parser + CSRF *)
let make_app = fun () ->
  match Middleware.session ~secret:"development-secret-key-change-in-production" () with
  | Error error -> Error (Middleware.Session.setup_error_to_string error)
  | Ok session_middleware ->
      Ok Middleware.[
        logger;
        session_middleware;
        body_parser ();
        csrf ~skip:(fun conn -> String.starts_with ~prefix:"/api/" (Conn.path conn)) ();
        router routes;
      ]

let main ~args:_ =
  let port = 8_080 in
  match Suri.config ~port () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      Log.info "===========================================";
      Log.info "🚀 Body Parser + CSRF Example";
      Log.info "===========================================";
      Log.info (String.concat "" [ "Server: http://localhost:"; string_of_int port ]);
      Log.info "✨ Features:";
      Log.info "  - HTML form with CSRF protection";
      Log.info "  - JSON API endpoint (/api/data)";
      Log.info "  - Automatic body parsing (urlencoded & JSON)";
      Log.info "===========================================";
      match make_app () with
      | Error error -> Error (Failure error)
      | Ok app ->
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
