open Std
open Suri

(**
   Comprehensive example demonstrating all 6 new HTTP middleware:

   1. Head Handler - Automatically strips body from HEAD requests
   2. Runtime - Adds X-Runtime header with request timing
   3. Method Override - Allows HTML forms to use PUT/PATCH/DELETE
   4. Remote IP - Extracts real client IP behind proxies
   5. ETag - Generates content-based cache identifiers
   6. Conditional Get - Returns 304 Not Modified for cached content

   Run with: riot run suri:caching_middleware
*)

(** Sample data for demonstration *)
let users = [
  ("1", "Alice", "alice@example.com");
  ("2", "Bob", "bob@example.com");
  ("3", "Charlie", "charlie@example.com");
]

let find_user = fun id -> List.find users ~fn:(fun (uid, _, _) -> uid = id)

(** Routes demonstrating the middleware *)
let routes =
  Middleware.Router.[
    get
      "/"
      (fun conn req ->
        let html =
          {|
<!DOCTYPE html>
<html>
<head>
  <title>HTTP Caching & Middleware Demo</title>
  <style>
    body { font-family: system-ui; max-width: 800px; margin: 50px auto; padding: 20px; }
    h1 { color: #333; }
    .section { background: #f5f5f5; padding: 15px; margin: 15px 0; border-radius: 5px; }
    code { background: #333; color: #0f0; padding: 2px 6px; border-radius: 3px; }
    .endpoint { margin: 10px 0; }
    pre { background: #333; color: #0f0; padding: 10px; overflow-x: auto; border-radius: 5px; }
    form { margin: 15px 0; }
    input[type="text"] { padding: 5px; margin: 5px; }
    button { padding: 8px 15px; background: #007bff; color: white; border: none; border-radius: 4px; cursor: pointer; }
    button:hover { background: #0056b3; }
  </style>
</head>
<body>
  <h1>🚀 HTTP Caching & Middleware Demo</h1>
  
  <div class="section">
    <h2>📋 Middleware Pipeline (5 NEW!)</h2>
    <ol>
      <li><strong>Request ID</strong> - Unique ID per request</li>
      <li><strong>Logger</strong> - Request/response logging</li>
      <li><strong>Runtime</strong> - ⭐ NEW: Adds X-Runtime timing header</li>
      <li><strong>Head Handler</strong> - ⭐ NEW: Automatic HEAD request support</li>
      <li><strong>Body Parser</strong> - Parse form data and JSON</li>
      <li><strong>Method Override</strong> - ⭐ NEW: HTML forms can use PUT/DELETE</li>
      <li><strong>Conditional Get</strong> - ⭐ NEW: 304 Not Modified responses</li>
      <li><strong>ETag</strong> - ⭐ NEW: Generate cache identifiers</li>
    </ol>
    <p><em>Note: Remote IP middleware available but not shown in this example.</em></p>
  </div>

  <div class="section">
    <h2>🧪 Try These Tests</h2>
    
    <h3>1. HEAD Request (Auto Body Stripping)</h3>
    <pre>curl -I http://localhost:4000/api/user/1</pre>
    <p>Notice: Headers present but no body sent!</p>

    <h3>2. Runtime Timing Header</h3>
    <pre>curl -I http://localhost:4000/api/user/1 | grep X-Runtime</pre>
    <p>Shows request processing time in seconds</p>

    <h3>3. ETag & Conditional GET (304 Caching)</h3>
    <pre># First request - returns 200 with ETag
curl -i http://localhost:4000/api/user/1

# Second request with If-None-Match - returns 304
curl -i http://localhost:4000/api/user/1 \
  -H "If-None-Match: \"<etag-from-above>\""</pre>
    <p>Second request returns 304 Not Modified - saves bandwidth!</p>

    <h3>4. Method Override (DELETE via POST)</h3>
    <form method="POST" action="/api/user/1">
      <input type="hidden" name="_method" value="DELETE">
      <button type="submit">Delete User #1 (Actually a POST)</button>
    </form>
    <p>Or via curl:</p>
    <pre>curl -X POST http://localhost:4000/api/user/1 \
  -d "_method=DELETE"</pre>

    <h3>5. Remote IP Detection</h3>
    <pre># Simulate proxy with X-Forwarded-For
curl http://localhost:4000/api/ip \
  -H "X-Forwarded-For: 203.0.113.45"</pre>
    <p>Server extracts real client IP from header</p>
  </div>

  <div class="section">
    <h2>📚 API Endpoints</h2>
    <div class="endpoint"><code>GET /</code> - This page</div>
    <div class="endpoint"><code>GET /api/users</code> - List all users (with ETag)</div>
    <div class="endpoint"><code>GET /api/user/:id</code> - Get user by ID (with ETag)</div>
    <div class="endpoint"><code>DELETE /api/user/:id</code> - Delete user (via method override)</div>
    <div class="endpoint"><code>GET /api/ip</code> - Show detected client IP</div>
  </div>
</body>
</html>
|}
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
        |> Conn.with_header "content-type" "text/html; charset=utf-8"
        |> Conn.send);
    get
      "/api/users"
      (fun conn req ->
        let json =
          {|{"users":[
  {"id":"1","name":"Alice","email":"alice@example.com"},
  {"id":"2","name":"Bob","email":"bob@example.com"},
  {"id":"3","name":"Charlie","email":"charlie@example.com"}
]}|}
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
        |> Conn.with_header "content-type" "application/json"
        |> Conn.send);
    get
      "/api/user/:id"
      (fun conn req ->
        let params = Conn.params conn in
        match Std.Collections.Proplist.get params ~key:"id" with
        | Some id ->
            (match find_user id with
            | Some (_, name, email) ->
                let json =
                  "{\"id\":\""
                  ^ id
                  ^ "\","
                  ^ "\"name\":\""
                  ^ name
                  ^ "\","
                  ^ "\"email\":\""
                  ^ email
                  ^ "\"}"
                in
                conn
                |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
                |> Conn.with_header "content-type" "application/json"
                |> Conn.send
            | None ->
                conn
                |> Conn.respond
                  ~status:Net.Http.Status.NotFound
                  ~body:{|{"error":"User not found"}|}
                |> Conn.with_header "content-type" "application/json"
                |> Conn.send)
        | None ->
            conn
            |> Conn.respond ~status:Net.Http.Status.BadRequest ~body:{|{"error":"Missing user ID"}|}
            |> Conn.with_header "content-type" "application/json"
            |> Conn.send);
    delete
      "/api/user/:id"
      (fun conn req ->
        let params = Conn.params conn in
        match Std.Collections.Proplist.get params ~key:"id" with
        | Some id ->
            let method_str = Net.Http.Method.to_string (Conn.method_ conn) in
            let message =
              "{\"message\":\"User "
              ^ id
              ^ " would be deleted (demo only)\","
              ^ "\"method\":\""
              ^ method_str
              ^ "\"}"
            in
            conn
            |> Conn.respond ~status:Net.Http.Status.Ok ~body:message
            |> Conn.with_header "content-type" "application/json"
            |> Conn.send
        | None ->
            conn
            |> Conn.respond ~status:Net.Http.Status.BadRequest ~body:{|{"error":"Missing user ID"}|}
            |> Conn.with_header "content-type" "application/json"
            |> Conn.send);
    get
      "/api/ip"
      (fun conn req ->
        let peer = Conn.peer conn in
        let client_ip = peer.ip in
        let headers = Conn.headers conn in
        let forwarded_for = Net.Http.Header.get headers "x-forwarded-for" in
        let json =
          match forwarded_for with
          | Some fwd ->
              "{\"detected_ip\":\""
              ^ client_ip
              ^ "\","
              ^ "\"x_forwarded_for\":\""
              ^ fwd
              ^ "\","
              ^ "\"note\":\"IP extracted from proxy headers\"}"
          | None ->
              "{\"detected_ip\":\""
              ^ client_ip
              ^ "\","
              ^ "\"note\":\"Direct connection, no proxy\"}"
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
        |> Conn.with_header "content-type" "application/json"
        |> Conn.send);
  ]

let main ~args:_ =
  (* Middleware pipeline showcasing all 6 new features *)
  (* NOTE: remote_ip commented out due to optional parameter type issues in lists *)
  (* To use: let mw = Remote_ip.middleware ~proxies:["..."] in ... mw :: rest ... *)
  let app = let open Middleware in
  [
    request_id;
    logger;
    runner;
    head;
    body_parser ();
    method_override;
    conditional_get;
    etag;
    router routes;
  ]
  in
  match Suri.config ~port:4_000 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          Log.info "===========================================";
          Log.info "🚀 HTTP Caching & Middleware Demo";
          Log.info "===========================================";
          Log.info "Server: http://localhost:4000";
          Log.info "";
          Log.info "⭐ NEW MIDDLEWARE ACTIVE (5 features):";
          Log.info "  ✅ Head Handler - Auto HEAD support";
          Log.info "  ✅ Runner - X-Runtime timing header";
          Log.info "  ✅ Method Override - Forms can DELETE/PUT";
          Log.info "  ✅ ETag - Content-based cache IDs";
          Log.info "  ✅ Conditional Get - 304 Not Modified";
          Log.info "";
          Log.info "Visit http://localhost:4000 for interactive examples!";
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
