# Riot ML Web & Network Programming Guide
## A Comprehensive Guide to Building Web Applications and Network Services

**Last Updated:** November 2025  
**For:** Developers building web applications and network services in Riot ML  
**What:** Complete walkthrough of HTTP, networking, databases, and web frameworks

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [HTTP Basics with Std.Net.Http](#http-basics-with-stdnethttp)
3. [HTTP Client with Blink](#http-client-with-blink)
4. [Web Server with Suri](#web-server-with-suri)
   - Architecture Overview
   - Basic Servers
   - Middleware Pipeline
   - Routing with Parameters
5. [Type-Safe UI Components with Suri](#type-safe-ui-components-with-suri)
   - Component Basics
   - Reusable Components
   - Attributes and Styling
   - Conditional Rendering
   - LiveView Interactive Components
6. [TCP Networking with Std.Net](#tcp-networking-with-stdnet)
7. [Database Access with Sqlx & Postgres](#database-access-with-sqlx--postgres)
8. [Building REST APIs](#building-rest-apis)
9. [WebSockets](#websockets)
10. [Real-World Examples](#real-world-examples)
11. [Available Examples](#available-examples)

---

## Quick Start

### Simple HTTP Client

```ocaml
open Std

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    (* Parse URL *)
    let uri = Net.Uri.of_string "https://api.example.com/users"
      |> Result.expect ~msg:"Invalid URL" in
    
    (* Connect and request *)
    let conn = Blink.connect uri
      |> Result.expect ~msg:"Connection failed" in
    
    let req = Net.Http.Request.get uri in
    Blink.request conn req () |> ignore;
    
    let response, body = Blink.await conn
      |> Result.expect ~msg:"Request failed" in
    
    println "Status: %d" (Net.Http.Status.to_int (Net.Http.Response.status response));
    println "Body: %s" body;
    
    Blink.close conn;
    Ok ()
  ) ~args:Env.args ()
```

### Simple Web Server

```ocaml
open Std
open Suri

let handle_request conn req =
  let uri = WebServer.Request.uri req in
  let method_ = WebServer.Request.method_ req in
  
  Log.info "%s %s" (Net.Http.Method.to_string method_) uri;
  
  WebServer.Response.ok ~body:"Hello, World!" ()

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    let config = WebServer.Config.make () in
    let handler_state = WebServer.Http1.make ~config ~handler:handle_request () in
    
    SocketPool.start_link 
      ~port:8080 
      ~handler:(module WebServer.Http1) 
      ~initial_state:handler_state;
    
    Log.info "Server listening on http://localhost:8080";
    Ok ()
  ) ~args:Env.args ()
```

---

## HTTP Basics with Std.Net.Http

### Core HTTP Types

Riot provides rich HTTP types in `Std.Net.Http`:

```ocaml
open Std.Net.Http

(* Status codes *)
Status.Ok                    (* 200 *)
Status.Created               (* 201 *)
Status.No_content            (* 204 *)
Status.Bad_request           (* 400 *)
Status.Not_found             (* 404 *)
Status.Internal_server_error (* 500 *)

(* HTTP methods *)
Method.Get
Method.Post
Method.Put
Method.Delete
Method.Patch
Method.Head
Method.Options

(* HTTP versions *)
Version.Http10
Version.Http11
Version.Http2
Version.Http3
```

### Working with URIs

```ocaml
open Std

(* Parse a URI *)
let uri = Net.Uri.of_string "https://api.example.com:443/v1/users?page=1"
  |> Result.expect ~msg:"Invalid URI" in

(* Extract components *)
let scheme = Net.Uri.scheme uri in      (* Some "https" *)
let host = Net.Uri.host uri in          (* Some "api.example.com" *)
let port = Net.Uri.port uri in          (* Some 443 *)
let path = Net.Uri.path uri in          (* "/v1/users" *)
let query = Net.Uri.query uri in        (* Some "page=1" *)

(* Build a URI *)
let uri = Net.Uri.make ()
  |> Net.Uri.with_scheme "https"
  |> Net.Uri.with_host "api.example.com"
  |> Net.Uri.with_port 443
  |> Net.Uri.with_path "/v1/users"
  |> Net.Uri.with_query "page=1" in

(* Convert to string *)
let url = Net.Uri.to_string uri in
println "URL: %s" url
```

### HTTP Headers

```ocaml
open Std.Net.Http

(* Create headers *)
let headers = Header.empty
  |> Header.set "Content-Type" "application/json"
  |> Header.set "Authorization" "Bearer token123"
  |> Header.set "User-Agent" "MyApp/1.0" in

(* Add headers (allows duplicates) *)
let headers = headers
  |> Header.add "Accept" "application/json"
  |> Header.add "Accept" "text/html" in

(* Get headers *)
match Header.get headers "Content-Type" with
| Some value -> println "Content-Type: %s" value
| None -> println "No Content-Type header"

(* Get all values *)
let accepts = Header.get_all headers "Accept" in
(* ["application/json"; "text/html"] *)

(* Check existence *)
if Header.has headers "Authorization" then
  println "Request is authenticated"

(* Iterate *)
Header.iter (fun name value ->
  println "%s: %s" name value
) headers;

(* Remove headers *)
let headers = Header.remove headers "X-Debug" in
```

### Creating Requests

```ocaml
open Std.Net.Http

(* Simple GET *)
let uri = Uri.of_string "https://api.example.com/users" |> Result.unwrap in
let req = Request.get uri in

(* POST with body *)
let body = {|{"name":"Alice","email":"alice@example.com"}|} in
let req = Request.post uri body
  |> Request.with_header "Content-Type" "application/json"
  |> Request.with_header "Accept" "application/json" in

(* PUT request *)
let req = Request.put uri {|{"name":"Bob"}|}
  |> Request.with_header "Content-Type" "application/json" in

(* DELETE request *)
let req = Request.delete uri
  |> Request.with_header "Authorization" "Bearer token" in

(* Using the builder pattern *)
let req = Request.Builder.create Method.Post uri
  |> Request.Builder.header "Content-Type" "application/json"
  |> Request.Builder.header "Authorization" "Bearer token"
  |> Request.Builder.body {|{"data":"value"}|}
  |> Request.Builder.build in

(* Inspecting requests *)
let method_ = Request.method_ req in
let uri = Request.uri req in
let headers = Request.headers req in
let body = Request.body req in  (* string option *)
```

### Creating Responses

```ocaml
open Std.Net.Http

(* Success responses *)
let resp = Response.ok {|{"status":"success"}|}
  |> Response.with_header "Content-Type" "application/json" in

let resp = Response.created {|{"id":123}|}
  |> Response.with_header "Location" "/users/123" in

let resp = Response.no_content () in

(* Error responses *)
let resp = Response.bad_request "Invalid JSON" in
let resp = Response.unauthorized "Authentication required" in
let resp = Response.forbidden "Access denied" in
let resp = Response.not_found "User not found" in
let resp = Response.internal_server_error "Database error" in

(* Using builder pattern *)
let resp = Response.Builder.create Status.Ok
  |> Response.Builder.header "Content-Type" "application/json"
  |> Response.Builder.header "Cache-Control" "no-cache"
  |> Response.Builder.body {|{"data":"value"}|}
  |> Response.Builder.build in

(* Inspecting responses *)
let status = Response.status resp in
if Status.is_success status then
  println "Success!"
else if Status.is_client_error status then
  println "Client error"
else if Status.is_server_error status then
  println "Server error"
```

---

## HTTP Client with Blink

Blink is Riot's high-performance HTTP client with support for HTTP/1.1, HTTP/2, HTTPS, and WebSockets.

### Basic GET Request

```ocaml
open Std

let fetch_data () =
  (* Parse URL *)
  let uri = Net.Uri.of_string "https://api.github.com/users/ocaml"
    |> Result.expect ~msg:"Invalid URL" in
  
  (* Connect - automatically uses TLS for https:// *)
  let conn = Blink.connect uri
    |> Result.expect ~msg:"Connection failed" in
  
  (* Create and send request *)
  let req = Net.Http.Request.get uri
    |> Net.Http.Request.with_header "Accept" "application/json"
    |> Net.Http.Request.with_header "User-Agent" "Riot-Blink/1.0" in
  
  Blink.request conn req ()
    |> Result.expect ~msg:"Request failed";
  
  (* Wait for response *)
  let response, body = Blink.await conn
    |> Result.expect ~msg:"Failed to receive response" in
  
  let status = Net.Http.Response.status response in
  println "Status: %d" (Net.Http.Status.to_int status);
  
  (* Parse JSON response *)
  match Data.Json.parse body with
  | Ok json ->
      (* Process JSON... *)
      println "Success!"
  | Error e ->
      eprintln "JSON parse error: %s" e;
  
  Blink.close conn
```

### POST Request with JSON

```ocaml
open Std

let create_user ~name ~email =
  let uri = Net.Uri.of_string "https://api.example.com/users"
    |> Result.expect ~msg:"Invalid URL" in
  
  (* Prepare JSON body *)
  let json_data = Data.Json.Object [
    ("name", Data.Json.String name);
    ("email", Data.Json.String email);
  ] in
  let body = Data.Json.to_string json_data in
  
  (* Connect *)
  let conn = Blink.connect uri
    |> Result.expect ~msg:"Connection failed" in
  
  (* Create POST request *)
  let req = Net.Http.Request.post uri body
    |> Net.Http.Request.with_header "Content-Type" "application/json"
    |> Net.Http.Request.with_header "Accept" "application/json" in
  
  Blink.request conn req ()
    |> Result.expect ~msg:"Request failed";
  
  let response, response_body = Blink.await conn
    |> Result.expect ~msg:"Failed to receive response" in
  
  let status = Net.Http.Response.status response in
  if Net.Http.Status.is_success status then
    println "User created successfully!"
  else
    eprintln "Failed to create user: %d" (Net.Http.Status.to_int status);
  
  Blink.close conn
```

### Error Handling

```ocaml
open Std

let fetch_with_error_handling uri_str =
  match Net.Uri.of_string uri_str with
  | Error _ ->
      eprintln "Invalid URL";
      Error "Invalid URL"
  | Ok uri ->
      match Blink.connect uri with
      | Error (Blink.Error.Net_error Net.Connection_refused) ->
          eprintln "Connection refused";
          Error "Connection refused"
      | Error (Blink.Error.Tls_error (Net.TlsStream.Handshake_failed msg)) ->
          eprintln "TLS handshake failed: %s" msg;
          Error "TLS error"
      | Error (Blink.Error.Protocol_error msg) ->
          eprintln "Protocol error: %s" msg;
          Error "Protocol error"
      | Error _ ->
          eprintln "Connection failed";
          Error "Connection failed"
      | Ok conn ->
          let req = Net.Http.Request.get uri in
          match Blink.request conn req () with
          | Error _ ->
              Blink.close conn;
              Error "Request failed"
          | Ok () ->
              match Blink.await conn with
              | Error _ ->
                  Blink.close conn;
                  Error "No response"
              | Ok (response, body) ->
                  Blink.close conn;
                  Ok (response, body)
```

### Streaming Responses

For large responses, you can stream data instead of waiting for the full body:

```ocaml
open Std

let stream_data uri =
  let conn = Blink.connect uri
    |> Result.expect ~msg:"Connection failed" in
  
  let req = Net.Http.Request.get uri in
  Blink.request conn req () |> ignore;
  
  (* Stream messages as they arrive *)
  let rec read_chunks () =
    match Blink.stream conn with
    | Error _ ->
        println "End of stream"
    | Ok messages ->
        List.iter (function
          | Blink.Connection.Response_headers (status, headers) ->
              println "Status: %d" (Net.Http.Status.to_int status)
          | Blink.Connection.Body_chunk data ->
              println "Received %d bytes" (String.length data);
              (* Process chunk... *)
          | Blink.Connection.Response_complete ->
              println "Response complete"
        ) messages;
        if List.exists (function
          | Blink.Connection.Response_complete -> true
          | _ -> false
        ) messages then ()
        else read_chunks ()
  in
  
  read_chunks ();
  Blink.close conn
```

---

## Web Server with Suri

**Suri** is Riot's modern, actor-based web framework providing:

✅ **Actor-Based Concurrency** - Built on Miniriot's lightweight processes with supervised connection pools

✅ **Type-Safe Components** - React-style component system for building UIs that work with static HTML or LiveView

✅ **Composable Middleware** - Router with parameter extraction, pipeline-based request processing

✅ **Production Ready** - HTTP/1.1 with keep-alive, WebSocket support via Channel API, fault tolerance

### Architecture Overview

Suri uses a **supervision tree** for fault tolerance:

```
WebServer.Supervisor
  ├── SocketPool.Supervisor
  │   ├── Acceptor 1
  │   ├── Acceptor 2
  │   └── ... (configurable)
  └── Connection Handlers (dynamic)
```

**Request Flow:**
```
TCP Accept → Parse HTTP → Middleware Pipeline → Handler → Send Response
```

### Quick Start - Hello World

```ocaml
open Std
open Suri

let handler _conn _req =
  WebServer.Response.ok ~body:"Hello, World!" ()

let () = run_with @@ fun () ->
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok _supervisor ->
      Log.info "Server running on http://0.0.0.0:8080";
      receive_any ()  (* Keep alive *)
  | Error `Bind_error ->
      Error (Failure "Failed to bind")
```

### Basic Server with Pattern Matching

```ocaml
open Std
open Suri

let handler _conn req =
  let uri = WebServer.Request.uri req in
  let method_ = WebServer.Request.method_ req in
  
  Log.info "%s %s" (Net.Http.Method.to_string method_) uri;
  
  match method_, uri with
  | Net.Http.Method.Get, "/" ->
      WebServer.Response.ok ~body:"Welcome!" ()
  | Net.Http.Method.Get, "/health" ->
      WebServer.Response.ok ~body:{|{"status":"ok"}|}
        ~headers:[("Content-Type", "application/json")]
        ()
  | Net.Http.Method.Post, "/api/echo" ->
      let body = WebServer.Request.body req in
      WebServer.Response.ok ~body ()
  | _ ->
      WebServer.Response.not_found ~body:"Not Found" ()

let () = run_with @@ fun () ->
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok _supervisor ->
      Log.info "Server listening on http://localhost:8080";
      receive_any ()
  | Error `Bind_error ->
      Error (Failure "Failed to bind")
```

### Middleware Pipeline

Suri provides a **composable middleware pipeline** system where each middleware can transform the connection context. The `Conn` (Connection) object flows through the pipeline, carrying request data and building up the response.

#### Understanding Conn (Connection Context)

The `Conn` module provides a rich context object:

```ocaml
open Suri.Middleware

(* Request Access *)
let method_ = Conn.method_ conn      (* HTTP method *)
let uri = Conn.uri conn              (* Full URI *)
let path = Conn.path conn            (* Path without query *)
let headers = Conn.headers conn      (* Request headers *)
let body = Conn.body conn            (* Request body *)
let params = Conn.params conn        (* Path/query params *)
let peer = Conn.peer conn            (* Client IP/port *)

(* Response Building *)
let conn = conn
  |> Conn.with_status Net.Http.Status.Ok
  |> Conn.with_body "Hello"
  |> Conn.with_header "Content-Type" "text/plain"

(* Or use convenience function *)
let conn = Conn.respond ~status:Net.Http.Status.Ok ~body:"Hello" conn

(* Control Flow *)
let conn = Conn.halt conn            (* Stop pipeline *)
let is_halted = Conn.halted conn     (* Check if halted *)
let is_sent = Conn.sent conn         (* Check if response sent *)
```

#### Example Middleware

```ocaml
open Std
open Suri.Middleware

(* Logger middleware - logs timing *)
let logger conn =
  let method_ = Conn.method_ conn in
  let uri = Conn.uri conn in
  let start = Time.Instant.now () in
  
  Log.info "%s %s" (Net.Http.Method.to_string method_) uri;
  
  (* Process request through rest of pipeline *)
  let conn = conn in
  
  (* Log completion time *)
  let elapsed = Time.Instant.elapsed start in
  let duration_ms = Time.Duration.to_millis elapsed in
  Log.info "Completed in %.2fms" duration_ms;
  
  conn

(* Authentication middleware - checks Bearer token *)
let require_auth conn =
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "Authorization" with
  | Some token when String.starts_with ~prefix:"Bearer " token ->
      conn  (* Authenticated - continue pipeline *)
  | _ ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Unauthorized 
          ~body:"Unauthorized"
      |> Conn.halt  (* Stop pipeline execution *)

(* CORS middleware - adds CORS headers *)
let cors conn =
  conn
  |> Conn.with_header "Access-Control-Allow-Origin" "*"
  |> Conn.with_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE"
  |> Conn.with_header "Access-Control-Allow-Headers" "Content-Type, Authorization"

(* JSON middleware - sets Content-Type *)
let json_middleware conn =
  conn |> Conn.with_header "Content-Type" "application/json"

(* Combine middleware into pipeline *)
let create_pipeline routes =
  Middleware.Pipeline.create ()
  |> Middleware.Pipeline.plug logger
  |> Middleware.Pipeline.plug cors
  |> Middleware.Pipeline.plug require_auth
  |> Middleware.Pipeline.plug (Middleware.Router.create routes)
  |> Middleware.Pipeline.to_handler
```

#### Using the Pipeline

```ocaml
let routes = [
  (* Your routes here - see Router section below *)
]

let handler =
  create_pipeline routes

let () = run_with @@ fun () ->
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok _supervisor ->
      Log.info "Server with middleware running on http://0.0.0.0:8080";
      receive_any ()
  | Error `Bind_error ->
      Error (Failure "Failed to bind")
```

### Routing with Parameter Extraction

Suri's Router provides **pattern-based routing** with automatic parameter extraction from URLs. Use `:param` syntax to capture path segments.

#### Basic Routes

```ocaml
open Std
open Suri.Middleware

let routes =
  let open Router in
  [
    get "/" (fun _conn _req ->
      WebServer.Response.ok ~body:"Home" ());
    
    get "/about" (fun _conn _req ->
      WebServer.Response.ok ~body:"About Us" ());
    
    post "/api/echo" (fun _conn req ->
      let body = WebServer.Request.body req in
      WebServer.Response.ok ~body ());
  ]
```

#### Routes with Parameters

Use `:param_name` to capture URL segments:

```ocaml
open Std
open Suri.Middleware

(* Extract parameter using Router's param helper *)
let handle_user _conn req =
  (* Parameters are extracted by the router *)
  let user_id = Router.param req "id" in  (* Gets value from /users/:id *)
  let body = Printf.sprintf {|{"user_id":"%s"}|} user_id in
  WebServer.Response.ok
    ~headers:[("Content-Type", "application/json")]
    ~body
    ()

let handle_post _conn req =
  let user_id = Router.param req "user_id" in
  let post_id = Router.param req "post_id" in
  let body = Printf.sprintf {|{"user":"%s","post":"%s"}|} user_id post_id in
  WebServer.Response.ok
    ~headers:[("Content-Type", "application/json")]
    ~body
    ()

let routes =
  let open Router in
  [
    (* Single parameter *)
    get "/users/:id" handle_user;
    
    (* Multiple parameters *)
    get "/users/:user_id/posts/:post_id" handle_post;
    
    (* Optional query parameters accessed from request *)
    get "/search" (fun _conn req ->
      let query = WebServer.Request.query req in
      (* Parse ?q=value from query string *)
      WebServer.Response.ok ~body:"Search results" ());
  ]
```

#### REST API Example

```ocaml
open Std
open Suri.Middleware

(* List all users *)
let list_users _conn _req =
  (* Fetch from database... *)
  let users_json = {|[{"id":1,"name":"Alice"},{"id":2,"name":"Bob"}]|} in
  WebServer.Response.ok
    ~headers:[("Content-Type", "application/json")]
    ~body:users_json
    ()

(* Get single user *)
let get_user _conn req =
  let id = Router.param req "id" in
  (* Fetch user by id... *)
  let user_json = Printf.sprintf {|{"id":"%s","name":"Alice"}|} id in
  WebServer.Response.ok
    ~headers:[("Content-Type", "application/json")]
    ~body:user_json
    ()

(* Create user *)
let create_user _conn req =
  let body = WebServer.Request.body req in
  
  match Data.Json.parse body with
  | Error e ->
      WebServer.Response.bad_request ~body:("Invalid JSON: " ^ e) ()
  | Ok json ->
      (* Validate and create user... *)
      let new_user_id = "123" in
      WebServer.Response.created
        ~headers:[
          ("Content-Type", "application/json");
          ("Location", "/users/" ^ new_user_id);
        ]
        ~body:(Printf.sprintf {|{"id":"%s"}|} new_user_id)
        ()

(* Update user *)
let update_user _conn req =
  let id = Router.param req "id" in
  let body = WebServer.Request.body req in
  
  match Data.Json.parse body with
  | Error e ->
      WebServer.Response.bad_request ~body:("Invalid JSON: " ^ e) ()
  | Ok json ->
      (* Update user... *)
      WebServer.Response.ok
        ~headers:[("Content-Type", "application/json")]
        ~body:(Printf.sprintf {|{"id":"%s","updated":true}|} id)
        ()

(* Delete user *)
let delete_user _conn req =
  let id = Router.param req "id" in
  (* Delete user... *)
  WebServer.Response.no_content ()

let routes =
  let open Router in
  [
    (* User CRUD *)
    get "/users" list_users;
    get "/users/:id" get_user;
    post "/users" create_user;
    put "/users/:id" update_user;
    delete "/users/:id" delete_user;
    
    (* Health check *)
    get "/health" (fun _conn _req ->
      WebServer.Response.ok
        ~headers:[("Content-Type", "application/json")]
        ~body:{|{"status":"ok"}|}
        ());
  ]

let handler =
  Middleware.Pipeline.create ()
  |> Middleware.Pipeline.plug (Middleware.Router.create routes)
  |> Middleware.Pipeline.to_handler
```

#### Scoped Routes (API Versioning)

Group routes under a common prefix:

```ocaml
let routes =
  let open Router in
  [
    (* Root level *)
    get "/" (fun _conn _req ->
      WebServer.Response.ok ~body:"Home" ());
    
    (* API v1 *)
    scope "/api/v1" [
      get "/users" list_users_v1;
      get "/users/:id" get_user_v1;
    ];
    
    (* API v2 with different handlers *)
    scope "/api/v2" [
      get "/users" list_users_v2;
      get "/users/:id" get_user_v2;
    ];
  ]
```

---

## Type-Safe UI Components with Suri

Suri provides a **React-style component system** for building type-safe HTML UIs. Components work with both static HTML rendering and interactive LiveView applications.

### Why Use Components?

✅ **Write Once, Render Anywhere** - Same components work for static HTML and LiveView
✅ **Type Safety** - Catch HTML errors at compile time
✅ **Composability** - Build reusable component libraries
✅ **No JavaScript Required** - Event handlers run on the server (LiveView)

### Basic Component Example

```ocaml
open Std
open Suri.Component

let welcome_page : unit t =
  html [
    head [
      title_ [text "Welcome"];
      meta ~attrs:[attr "charset" "UTF-8"] ();
    ];
    body [
      div ~attrs:[class_ "container"] [
        h1 [text "Welcome to Suri"];
        p [text "Build type-safe web apps with OCaml"];
        button ~attrs:[class_ "btn"] [text "Get Started"];
      ];
    ];
  ]

let handler _conn _req =
  let html_string = to_html welcome_page in
  WebServer.Response.ok
    ~headers:[("Content-Type", "text/html")]
    ~body:html_string
    ()
```

### Reusable Components

```ocaml
open Suri.Component

(* Reusable card component *)
let card ~title ~content =
  div ~attrs:[class_ "card"] [
    h3 ~attrs:[class_ "card-title"] [text title];
    p ~attrs:[class_ "card-content"] [text content];
  ]

(* Reusable button component *)
let button ~label ~variant =
  let class_name = match variant with
    | `Primary -> "btn btn-primary"
    | `Secondary -> "btn btn-secondary"
    | `Danger -> "btn btn-danger"
  in
  button ~attrs:[class_ class_name] [text label]

(* Use components *)
let my_page =
  html [
    body [
      card ~title:"Getting Started" ~content:"Welcome to our app";
      card ~title:"Features" ~content:"Explore what we offer";
      button ~label:"Sign Up" ~variant:`Primary;
    ];
  ]
```

### Component Categories

Suri provides **115+ HTML5 elements**:

**Document Structure:**
- `html`, `head`, `body`, `title_`, `meta`, `link`, `style_`, `script`

**Text Content:**
- `div`, `span`, `p`, `h1`-`h6`, `blockquote`, `pre`, `code`

**Lists:**
- `ul`, `ol`, `li`, `dl`, `dt`, `dd`

**Forms:**
- `form`, `input`, `textarea`, `select`, `option`, `button`, `label`, `fieldset`

**Tables:**
- `table`, `thead`, `tbody`, `tr`, `th`, `td`, `caption`, `colgroup`, `col`

**Media:**
- `img`, `audio`, `video`, `source`, `track`, `canvas`

**Semantic HTML5:**
- `article`, `section`, `nav`, `aside`, `header`, `footer`, `main`, `figure`

**Interactive:**
- `details`, `summary`, `dialog`, `menu`

### Attributes and Styling

```ocaml
open Suri.Component

(* Common attributes *)
let element =
  div ~attrs:[
    id "my-div";
    class_ "container mx-auto";
    style "background-color: blue; padding: 20px";
    attr "data-value" "123";  (* Custom attributes *)
  ] [
    text "Content"
  ]

(* Links and images *)
let link_example =
  a ~attrs:[href "/about"; target "_blank"] [
    text "About Us"
  ]

let image_example =
  img ~attrs:[
    src "/logo.png";
    alt "Company Logo";
    attr "width" "200";
    attr "height" "100";
  ] ()

(* Form inputs *)
let form_example =
  form ~attrs:[action "/submit"; method_ "POST"] [
    label ~attrs:[for_ "email"] [text "Email:"];
    input ~attrs:[
      type_ "email";
      id "email";
      name "email";
      placeholder "user@example.com";
      required;
    ] ();
    
    button ~attrs:[type_ "submit"] [text "Submit"];
  ]
```

### Conditional Rendering

```ocaml
open Suri.Component

let user_profile ~user ~is_admin =
  div [
    h2 [text user.name];
    
    (* Conditionally show admin badge *)
    when_ is_admin (
      span ~attrs:[class_ "badge badge-admin"] [text "Admin"]
    );
    
    (* Unless helper *)
    unless user.email_verified (
      div ~attrs:[class_ "alert alert-warning"] [
        text "Please verify your email"
      ]
    );
    
    (* Maybe helper for optional values *)
    maybe user.bio (fun bio ->
      p ~attrs:[class_ "bio"] [text bio]
    );
  ]
```

### Lists and Iteration

```ocaml
open Suri.Component

let user_list users =
  ul ~attrs:[class_ "user-list"] (
    List.map (fun user ->
      li ~attrs:[class_ "user-item"] [
        text user.name;
        text " - ";
        text user.email;
      ]
    ) users
  )

let table_example data =
  table ~attrs:[class_ "data-table"] [
    thead [
      tr [
        th [text "ID"];
        th [text "Name"];
        th [text "Email"];
      ];
    ];
    tbody (
      List.map (fun row ->
        tr [
          td [text (Int.to_string row.id)];
          td [text row.name];
          td [text row.email];
        ]
      ) data
    );
  ]
```

### LiveView - Interactive Components

LiveView enables server-side interactivity without JavaScript. Components send events to the server, which updates state and re-renders.

```ocaml
open Std
open Suri

module Counter = struct
  type state = { count: int }
  type msg = Increment | Decrement | Reset
  
  let init _conn = { count: 0 }
  
  let update msg state =
    match msg with
    | Increment -> { count = state.count + 1 }
    | Decrement -> { count = state.count - 1 }
    | Reset -> { count = 0 }
  
  let render ~state () =
    Component.(
      div ~attrs:[class_ "counter"] [
        h1 [text "Counter: "; text (Int.to_string state.count)];
        
        div ~attrs:[class_ "controls"] [
          button ~attrs:[on_click (fun _ -> Decrement)] [text "-"];
          button ~attrs:[on_click (fun _ -> Reset)] [text "Reset"];
          button ~attrs:[on_click (fun _ -> Increment)] [text "+"];
        ];
      ]
    )
end

(* Register LiveView route *)
let routes =
  let open Middleware.Router in
  [
    get "/" (fun _conn _req ->
      WebServer.Response.ok ~body:"Home" ());
    
    (* LiveView route *)
    LiveView.route "/counter" (module Counter);
  ]
```

**LiveView Features:**
- Event handlers: `on_click`, `on_submit`, `on_input`, `on_change`, `on_focus`, `on_blur`, etc.
- Server-side state management
- Automatic DOM diffing and patching
- WebSocket communication (transparent to developer)
- No build step or client-side framework

### Complete Component Example

```ocaml
open Std
open Suri
open Component

(* Design system components *)
module UI = struct
  let page ~title ~children =
    html [
      head [
        title_ [text title];
        meta ~attrs:[attr "charset" "UTF-8"] ();
        meta ~attrs:[
          name "viewport";
          content "width=device-width, initial-scale=1.0";
        ] ();
        link ~attrs:[
          rel "stylesheet";
          href "/static/style.css";
        ] ();
      ];
      body children;
    ]
  
  let navbar ~active_page =
    nav ~attrs:[class_ "navbar"] [
      a ~attrs:[href "/"; class_ (if active_page = "home" then "active" else "")] [
        text "Home"
      ];
      a ~attrs:[href "/about"; class_ (if active_page = "about" then "active" else "")] [
        text "About"
      ];
      a ~attrs:[href "/contact"; class_ (if active_page = "contact" then "active" else "")] [
        text "Contact"
      ];
    ]
  
  let card ~title ~content =
    div ~attrs:[class_ "card"] [
      h3 [text title];
      p [text content];
    ]
end

(* Use design system *)
let home_page =
  UI.page ~title:"Home" ~children:[
    UI.navbar ~active_page:"home";
    
    main ~attrs:[class_ "container"] [
      h1 [text "Welcome"];
      
      div ~attrs:[class_ "cards"] [
        UI.card ~title:"Feature 1" ~content:"Description...";
        UI.card ~title:"Feature 2" ~content:"Description...";
        UI.card ~title:"Feature 3" ~content:"Description...";
      ];
    ];
    
    footer ~attrs:[class_ "footer"] [
      text "© 2025 My Company";
    ];
  ]

let handler _conn _req =
  let html = to_html home_page in
  WebServer.Response.ok
    ~headers:[("Content-Type", "text/html; charset=utf-8")]
    ~body:html
    ()
```

---

## TCP Networking with Std.Net

### TCP Client

```ocaml
open Std

let tcp_client_example () =
  (* Create address *)
  let addr = Net.Addr.of_host_and_port 
    ~host:"example.com" 
    ~port:80
    |> Result.expect ~msg:"Invalid address" in
  
  (* Connect *)
  let stream = Net.TcpStream.connect addr
    |> Result.expect ~msg:"Connection failed" in
  
  Log.info "Connected!";
  
  (* Send HTTP request *)
  let request = "GET / HTTP/1.0\r\nHost: example.com\r\n\r\n" in
  let buf = Bytes.of_string request in
  
  let n = Net.TcpStream.write stream buf ()
    |> Result.expect ~msg:"Write failed" in
  
  Log.info "Sent %d bytes" n;
  
  (* Read response *)
  let response_buf = Bytes.create 4096 in
  let rec read_all acc =
    match Net.TcpStream.read stream response_buf () with
    | Ok 0 -> acc  (* EOF *)
    | Ok n ->
        let chunk = Bytes.sub_string response_buf 0 n in
        read_all (acc ^ chunk)
    | Error Net.Closed -> acc
    | Error e ->
        Log.error "Read failed";
        acc
  in
  
  let response = read_all "" in
  println "Response:\n%s" response;
  
  Net.TcpStream.close stream

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    spawn tcp_client_example |> ignore;
    Ok ()
  ) ~args:Env.args ()
```

### TCP Server

```ocaml
open Std

let handle_client stream client_addr =
  Log.info "Client connected from %s:%d" 
    (Net.Addr.ip client_addr) 
    (Net.Addr.port client_addr);
  
  (* Read request *)
  let buf = Bytes.create 1024 in
  match Net.TcpStream.read stream buf () with
  | Ok n ->
      let data = Bytes.sub_string buf 0 n in
      Log.info "Received: %s" data;
      
      (* Send response *)
      let response = "Echo: " ^ data in
      let response_buf = Bytes.of_string response in
      Net.TcpStream.write stream response_buf () |> ignore;
  | Error e ->
      Log.error "Read failed";
  
  Net.TcpStream.close stream;
  Ok ()

let tcp_server_example () =
  (* Bind to address *)
  let addr = Net.Addr.(tcp loopback 8080) in
  let listener = Net.TcpListener.bind ~reuse_addr:true addr
    |> Result.expect ~msg:"Failed to bind" in
  
  Log.info "Server listening on port 8080";
  
  (* Accept loop *)
  let rec accept_loop () =
    match Net.TcpListener.accept listener with
    | Ok (stream, client_addr) ->
        (* Spawn handler for each connection *)
        spawn (fun () -> handle_client stream client_addr) |> ignore;
        accept_loop ()
    | Error e ->
        Log.error "Accept failed";
        accept_loop ()
  in
  
  accept_loop ()

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    spawn tcp_server_example |> ignore;
    Ok ()
  ) ~args:Env.args ()
```

### TLS/HTTPS

```ocaml
open Std

let https_client_example () =
  (* Parse HTTPS URL *)
  let uri = Net.Uri.of_string "https://api.github.com"
    |> Result.expect ~msg:"Invalid URL" in
  
  (* TLS is handled automatically by Blink for https:// URLs *)
  let conn = Blink.connect uri
    |> Result.expect ~msg:"Connection failed" in
  
  let req = Net.Http.Request.get uri in
  Blink.request conn req () |> ignore;
  
  let response, body = Blink.await conn
    |> Result.expect ~msg:"Request failed" in
  
  println "Secure response received!";
  
  Blink.close conn

(* Or use TlsStream directly *)
let tls_stream_example () =
  let addr = Net.Addr.of_host_and_port 
    ~host:"api.github.com" 
    ~port:443
    |> Result.expect ~msg:"Invalid address" in
  
  (* Connect with TLS *)
  let stream = Net.TlsStream.connect ~hostname:"api.github.com" addr
    |> Result.expect ~msg:"TLS connection failed" in
  
  Log.info "TLS handshake complete!";
  
  (* Use like regular TcpStream *)
  let request = "GET / HTTP/1.1\r\nHost: api.github.com\r\n\r\n" in
  let buf = Bytes.of_string request in
  Net.TlsStream.write stream buf () |> ignore;
  
  (* Read response... *)
  
  Net.TlsStream.close stream
```

---

## Database Access with Sqlx & Postgres

Sqlx provides a database-agnostic interface with connection pooling, while Postgres is the PostgreSQL driver.

### Connecting to PostgreSQL

```ocaml
open Std

let connect_to_db () =
  (* Configure connection *)
  let config = Postgres.Config.{
    host = "localhost";
    port = 5432;
    database = "myapp";
    user = "postgres";
    password = "secret";
    ssl_mode = `Prefer;
    application_name = Some "my_app";
    connect_timeout = Time.Duration.of_sec 10;
    keepalives_idle = None;
  } in
  
  (* Create connection pool *)
  let pool_config = Sqlx.Config.{
    pool_size = 10;
    max_idle_time = Time.Duration.of_sec 300;
    acquire_timeout = Time.Duration.of_sec 5;
    idle_check_interval = Time.Duration.of_sec 30;
    max_lifetime = Some (Time.Duration.of_sec 3600);
    auto_commit = true;
    isolation_level = Some `Read_committed;
    query_timeout = Some (Time.Duration.of_sec 30);
    log_queries = true;
    log_slow_queries = Some (Time.Duration.of_millis 100);
  } in
  
  Sqlx.connect 
    ~config:pool_config
    ~driver:(module Postgres.Driver) 
    config
    |> Result.expect ~msg:"Failed to connect to database"

(* Or use connection string *)
let connect_with_string () =
  let config = Postgres.Config.from_string 
    "postgresql://user:password@localhost:5432/mydb"
    |> Result.expect ~msg:"Invalid connection string" in
  
  Sqlx.connect 
    ~driver:(module Postgres.Driver) 
    config
    |> Result.expect ~msg:"Connection failed"
```

### Executing Queries

```ocaml
open Std

let query_examples pool =
  (* Simple query *)
  let cursor = Sqlx.query pool 
    "SELECT id, name, email FROM users" 
    []
    |> Result.expect ~msg:"Query failed" in
  
  (* Iterate results *)
  Sqlx.Cursor.iter (fun row ->
    let id = Sqlx.Row.get_int row 0 
      |> Option.expect ~msg:"Missing id" in
    let name = Sqlx.Row.get_string row 1 
      |> Option.expect ~msg:"Missing name" in
    let email = Sqlx.Row.get_string row 2 
      |> Option.expect ~msg:"Missing email" in
    
    println "User: %d - %s <%s>" id name email
  ) cursor;
  
  (* Query with parameters (prevents SQL injection) *)
  let cursor = Sqlx.query pool
    "SELECT * FROM users WHERE email = $1"
    [Sqlx.Value.string "alice@example.com"]
    |> Result.expect ~msg:"Query failed" in
  
  (* Get single row *)
  match Sqlx.Cursor.next cursor with
  | Some row ->
      let name = Sqlx.Row.get_string row 1 
        |> Option.unwrap_or ~default:"Unknown" in
      println "Found user: %s" name
  | None ->
      println "User not found"

(* Execute (INSERT, UPDATE, DELETE) *)
let execute_examples pool =
  (* Insert *)
  let rows_affected = Sqlx.exec pool
    "INSERT INTO users (name, email) VALUES ($1, $2)"
    [Sqlx.Value.string "Bob"; Sqlx.Value.string "bob@example.com"]
    |> Result.expect ~msg:"Insert failed" in
  
  println "Inserted %d rows" rows_affected;
  
  (* Update *)
  let rows_affected = Sqlx.exec pool
    "UPDATE users SET email = $1 WHERE name = $2"
    [Sqlx.Value.string "newemail@example.com"; Sqlx.Value.string "Bob"]
    |> Result.expect ~msg:"Update failed" in
  
  println "Updated %d rows" rows_affected;
  
  (* Delete *)
  let rows_affected = Sqlx.exec pool
    "DELETE FROM users WHERE name = $1"
    [Sqlx.Value.string "Bob"]
    |> Result.expect ~msg:"Delete failed" in
  
  println "Deleted %d rows" rows_affected
```

### Transactions

```ocaml
open Std

let transfer_money pool ~from_account ~to_account ~amount =
  Sqlx.with_transaction pool (fun conn ->
    (* Debit from source *)
    let rows = Sqlx.Connection.exec conn
      "UPDATE accounts SET balance = balance - $1 WHERE id = $2"
      [Sqlx.Value.int amount; Sqlx.Value.int from_account]
      |> Result.map_err (fun e -> Sqlx.show_error e) in
    
    match rows with
    | Error e -> Error e
    | Ok 0 -> Error "Source account not found"
    | Ok _ ->
        (* Credit to destination *)
        let rows = Sqlx.Connection.exec conn
          "UPDATE accounts SET balance = balance + $1 WHERE id = $2"
          [Sqlx.Value.int amount; Sqlx.Value.int to_account]
          |> Result.map_err (fun e -> Sqlx.show_error e) in
        
        match rows with
        | Error e -> Error e
        | Ok 0 -> Error "Destination account not found"
        | Ok _ -> Ok ()
  )
  |> Result.expect ~msg:"Transaction failed"

(* The transaction will automatically commit on Ok or rollback on Error *)
```

### Working with Values

```ocaml
open Std

let value_examples pool =
  (* Different value types *)
  let params = [
    Sqlx.Value.int 42;
    Sqlx.Value.string "text";
    Sqlx.Value.bool true;
    Sqlx.Value.float 3.14;
    Sqlx.Value.null;
  ] in
  
  (* Extract values from rows *)
  let cursor = Sqlx.query pool "SELECT * FROM data" []
    |> Result.expect ~msg:"Query failed" in
  
  Sqlx.Cursor.iter (fun row ->
    (* Get by index *)
    let id = Sqlx.Row.get_int row 0 in
    let name = Sqlx.Row.get_string row 1 in
    let active = Sqlx.Row.get_bool row 2 in
    let score = Sqlx.Row.get_float row 3 in
    
    match id, name, active, score with
    | Some id, Some name, Some active, Some score ->
        println "%d: %s (active=%b, score=%f)" id name active score
    | _ ->
        println "Some values were NULL"
  ) cursor
```

---

## Building REST APIs

### Complete REST API Example

```ocaml
open Std
open Suri.Middleware

(* Database models *)
type user = {
  id : int;
  name : string;
  email : string;
}

let user_to_json user =
  Data.Json.Object [
    ("id", Data.Json.Number (float_of_int user.id));
    ("name", Data.Json.String user.name);
    ("email", Data.Json.String user.email);
  ]

let user_from_json json =
  match json with
  | Data.Json.Object fields ->
      (match List.assoc_opt "name" fields, List.assoc_opt "email" fields with
       | Some (Data.Json.String name), Some (Data.Json.String email) ->
           Ok { id = 0; name; email }
       | _ -> Error "Missing required fields")
  | _ -> Error "Expected JSON object"

(* Database operations *)
let find_user pool id =
  let cursor = Sqlx.query pool
    "SELECT id, name, email FROM users WHERE id = $1"
    [Sqlx.Value.int id]
    |> Result.expect ~msg:"Query failed" in
  
  match Sqlx.Cursor.next cursor with
  | Some row ->
      (match Sqlx.Row.get_int row 0, 
             Sqlx.Row.get_string row 1,
             Sqlx.Row.get_string row 2 with
       | Some id, Some name, Some email ->
           Some { id; name; email }
       | _ -> None)
  | None -> None

let create_user pool user =
  let cursor = Sqlx.query pool
    "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id"
    [Sqlx.Value.string user.name; Sqlx.Value.string user.email]
    |> Result.expect ~msg:"Insert failed" in
  
  match Sqlx.Cursor.next cursor with
  | Some row ->
      (match Sqlx.Row.get_int row 0 with
       | Some id -> { user with id }
       | None -> user)
  | None -> user

(* API handlers *)
let list_users pool conn =
  let cursor = Sqlx.query pool
    "SELECT id, name, email FROM users"
    []
    |> Result.expect ~msg:"Query failed" in
  
  let users = ref [] in
  Sqlx.Cursor.iter (fun row ->
    match Sqlx.Row.get_int row 0,
          Sqlx.Row.get_string row 1,
          Sqlx.Row.get_string row 2 with
    | Some id, Some name, Some email ->
        users := { id; name; email } :: !users
    | _ -> ()
  ) cursor;
  
  let json = Data.Json.Array (List.map user_to_json !users) in
  let body = Data.Json.to_string json in
  
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body
  |> Conn.with_header "Content-Type" "application/json"

let get_user pool conn =
  let params = Conn.params conn in
  match List.assoc_opt "id" params with
  | None ->
      conn |> Conn.respond ~status:Net.Http.Status.Bad_request 
          ~body:"Missing user ID"
  | Some id_str ->
      (match int_of_string_opt id_str with
       | None ->
           conn |> Conn.respond ~status:Net.Http.Status.Bad_request 
               ~body:"Invalid user ID"
       | Some id ->
           match find_user pool id with
           | None ->
               conn |> Conn.respond ~status:Net.Http.Status.Not_found 
                   ~body:"User not found"
           | Some user ->
               let json = user_to_json user in
               let body = Data.Json.to_string json in
               conn
               |> Conn.respond ~status:Net.Http.Status.Ok ~body
               |> Conn.with_header "Content-Type" "application/json")

let create_user_handler pool conn =
  let body = Conn.body conn in
  
  match Data.Json.parse body with
  | Error e ->
      conn |> Conn.respond ~status:Net.Http.Status.Bad_request 
          ~body:("Invalid JSON: " ^ e)
  | Ok json ->
      match user_from_json json with
      | Error e ->
          conn |> Conn.respond ~status:Net.Http.Status.Bad_request ~body:e
      | Ok user ->
          let user = create_user pool user in
          let json = user_to_json user in
          let body = Data.Json.to_string json in
          
          conn
          |> Conn.respond ~status:Net.Http.Status.Created ~body
          |> Conn.with_header "Content-Type" "application/json"
          |> Conn.with_header "Location" 
              (Printf.sprintf "/users/%d" user.id)

(* Setup routes *)
let make_routes pool =
  Router.[
    get "/users" (list_users pool);
    get "/users/:id" (get_user pool);
    post "/users" (create_user_handler pool);
  ]

(* Main server *)
let () =
  Miniriot.run ~main:(fun ~args:_ ->
    (* Connect to database *)
    let config = Postgres.Config.default () in
    let pool = Sqlx.connect ~driver:(module Postgres.Driver) config
      |> Result.expect ~msg:"Database connection failed" in
    
    Log.info "Connected to database";
    
    (* Create routes *)
    let routes = make_routes pool in
    let app = [ Router.middleware routes ] in
    
    (* Start server *)
    let handler conn req =
      let conn = Conn.make conn req in
      let conn = Pipeline.run conn app in
      Conn.to_response conn
    in
    
    let config = Suri.WebServer.Config.make () in
    let handler_state = Suri.WebServer.Http1.make 
      ~config ~handler () in
    
    Suri.SocketPool.start_link 
      ~port:8080 
      ~handler:(module Suri.WebServer.Http1) 
      ~initial_state:handler_state;
    
    Log.info "API server listening on http://localhost:8080";
    Ok ()
  ) ~args:Env.args ()
```

---

## WebSockets

### WebSocket Server with Suri

```ocaml
open Std
open Suri

(* WebSocket handler *)
type Message.t +=
  | WsMessage of { data : string }
  | WsClose

let websocket_handler () =
  Log.info "WebSocket connection established";
  
  let rec loop () =
    let selector = function
      | WsMessage { data } -> Some (`Message data)
      | WsClose -> Some `Close
      | _ -> None
    in
    
    match receive ~selector () with
    | `Message data ->
        Log.info "Received: %s" data;
        
        (* Echo back *)
        let response = "Echo: " ^ data in
        (* Send response via WebSocket... *)
        
        loop ()
    | `Close ->
        Log.info "WebSocket closed";
        Ok ()
  in
  
  loop ()

(* Channel-based WebSocket *)
let setup_websocket () =
  let channel = Channel.create ~path:"/ws" 
    ~handler:websocket_handler in
  
  channel
```

---

## Real-World Examples

### Complete Web Application

```ocaml
open Std
open Suri.Middleware

(* Application state *)
type app_state = {
  db_pool : Sqlx.Pool.t;
  start_time : Time.Instant.t;
}

(* Middleware: Request logging *)
let request_logger conn =
  let method_ = Conn.method_ conn in
  let uri = Conn.uri conn in
  let peer = Conn.peer conn in
  let start = Time.Instant.now () in
  
  Log.info "%s:%d %s %s" 
    (Net.Addr.ip peer.ip) peer.port
    (Net.Http.Method.to_string method_) uri;
  
  let conn = conn in
  
  let elapsed = Time.Instant.elapsed start in
  let duration_ms = Time.Duration.to_millis elapsed in
  
  Log.info "Completed in %.2fms" duration_ms;
  conn

(* Middleware: CORS *)
let cors_middleware conn =
  conn
  |> Conn.with_header "Access-Control-Allow-Origin" "*"
  |> Conn.with_header "Access-Control-Allow-Methods" "GET, POST, PUT, DELETE, OPTIONS"
  |> Conn.with_header "Access-Control-Allow-Headers" "Content-Type, Authorization"

(* Middleware: JSON content type *)
let json_middleware conn =
  conn |> Conn.with_header "Content-Type" "application/json"

(* Routes *)
let health_check state conn =
  let uptime = Time.Instant.elapsed state.start_time in
  let uptime_secs = Time.Duration.to_secs uptime in
  
  let json = Data.Json.Object [
    ("status", Data.Json.String "ok");
    ("uptime_seconds", Data.Json.Number uptime_secs);
  ] in
  
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok 
      ~body:(Data.Json.to_string json)
  |> json_middleware

let make_app state =
  let routes = Router.[
    get "/health" (health_check state);
    get "/api/v1/users" (list_users state.db_pool);
    get "/api/v1/users/:id" (get_user state.db_pool);
    post "/api/v1/users" (create_user_handler state.db_pool);
  ] in
  
  [
    request_logger;
    cors_middleware;
    Router.middleware routes;
  ]

let () =
  Miniriot.run ~main:(fun ~args:_ ->
    Log.set_level Log.Info;
    
    (* Initialize database *)
    let db_config = Postgres.Config.default () in
    let pool = Sqlx.connect ~driver:(module Postgres.Driver) db_config
      |> Result.expect ~msg:"Database connection failed" in
    
    let state = {
      db_pool = pool;
      start_time = Time.Instant.now ();
    } in
    
    (* Build app *)
    let app = make_app state in
    
    let handler conn req =
      let conn = Conn.make conn req in
      let conn = Pipeline.run conn app in
      Conn.to_response conn
    in
    
    (* Start server *)
    let config = Suri.WebServer.Config.make () in
    let handler_state = Suri.WebServer.Http1.make 
      ~config ~handler () in
    
    Suri.SocketPool.start_link 
      ~port:8080 
      ~handler:(module Suri.WebServer.Http1) 
      ~initial_state:handler_state;
    
    Log.info "🚀 Server running on http://localhost:8080";
    Log.info "   - Health check: http://localhost:8080/health";
    Log.info "   - API: http://localhost:8080/api/v1/users";
    
    Ok ()
  ) ~args:Env.args ()
```

---

## Best Practices

### 1. Always Use Connection Pools

```ocaml
(* GOOD - reuse connections *)
let pool = Sqlx.connect ~driver:(module Postgres.Driver) config in

(* Use pool for all queries *)
Sqlx.query pool "SELECT ..." [] |> ignore

(* BAD - creating new connections *)
let query_data () =
  let pool = Sqlx.connect ~driver:(module Postgres.Driver) config in
  Sqlx.query pool "SELECT ..." []
```

### 2. Use Parameterized Queries

```ocaml
(* GOOD - safe from SQL injection *)
Sqlx.query pool
  "SELECT * FROM users WHERE email = $1"
  [Sqlx.Value.string email]

(* BAD - SQL injection vulnerability! *)
Sqlx.query pool
  (Printf.sprintf "SELECT * FROM users WHERE email = '%s'" email)
  []
```

### 3. Handle Errors Explicitly

```ocaml
(* GOOD *)
match Blink.connect uri with
| Ok conn -> process conn
| Error (Blink.Error.Net_error Net.Connection_refused) ->
    Log.error "Connection refused";
    (* Handle gracefully *)
| Error e ->
    Log.error "Connection failed";
    (* Handle other errors *)

(* AVOID - losing error information *)
let conn = Blink.connect uri |> Result.unwrap in
```

### 4. Use Middleware for Cross-Cutting Concerns

```ocaml
(* GOOD - middleware handles auth, logging, CORS *)
let app = [
  logger_middleware;
  cors_middleware;
  auth_middleware;
  Router.middleware routes;
]

(* BAD - mixing concerns in handlers *)
let handler conn =
  (* logging code *)
  (* auth code *)
  (* CORS code *)
  (* actual handler logic *)
  ...
```

### 5. Structure Large Applications

```
my-web-app/
├── src/
│   ├── main.ml              # Entry point
│   ├── config.ml            # Configuration
│   ├── database.ml          # Database setup
│   ├── models/              # Data models
│   │   ├── user.ml
│   │   └── post.ml
│   ├── handlers/            # HTTP handlers
│   │   ├── users.ml
│   │   └── posts.ml
│   ├── middleware/          # Custom middleware
│   │   ├── auth.ml
│   │   └── rate_limit.ml
│   └── routes.ml            # Route definitions
└── tusk.toml
```

---

## Quick Reference

### HTTP Status Codes

| Code | Constant | Use Case |
|------|----------|----------|
| 200 | `Status.Ok` | Successful GET, PUT, PATCH |
| 201 | `Status.Created` | Successful POST (resource created) |
| 204 | `Status.No_content` | Successful DELETE (no body) |
| 400 | `Status.Bad_request` | Invalid request data |
| 401 | `Status.Unauthorized` | Authentication required |
| 403 | `Status.Forbidden` | Authenticated but not authorized |
| 404 | `Status.Not_found` | Resource doesn't exist |
| 500 | `Status.Internal_server_error` | Server error |

### Common Headers

```ocaml
(* Content negotiation *)
"Content-Type" -> "application/json"
"Content-Type" -> "text/html"
"Accept" -> "application/json"

(* Authentication *)
"Authorization" -> "Bearer token123"
"WWW-Authenticate" -> "Bearer"

(* Caching *)
"Cache-Control" -> "no-cache"
"Cache-Control" -> "max-age=3600"
"ETag" -> "abc123"

(* CORS *)
"Access-Control-Allow-Origin" -> "*"
"Access-Control-Allow-Methods" -> "GET, POST, PUT, DELETE"
"Access-Control-Allow-Headers" -> "Content-Type"

(* Location *)
"Location" -> "/users/123"  (* For 201 Created *)
```

---

## Available Examples

All packages include runnable examples. Here's what's available:

### Suri Examples (packages/suri/examples/)

Run with: `tusk run suri:example_name`

**Basic Examples:**
- `hello_world.ml` - Minimal HTTP server
- `cors_simple.ml` - Simple CORS middleware example
- `csrf_simple.ml` - CSRF protection example
- `design_system.ml` - Component-based design system

**Component Examples:**
- `basic_component.ml` - Basic component usage
- `liveview_counter.ml` - Interactive counter with LiveView
- `liveview_multi.ml` - Multiple LiveView components
- `liveview_migration.ml` - LiveView migration example

**API Examples:**
- `json_api.ml` - RESTful JSON API with routing

**Developer Tools:**
- `debugger_test.ml` - Debugging utilities example

**Component Examples:**
- `basic_component.ml` - Full-page component example with forms
- `design_system.ml` - Reusable component library pattern

**LiveView Examples:**
- `liveview_counter.ml` - Interactive counter with buttons
- `liveview_migration.ml` - Static HTML → LiveView migration guide

**Advanced:**
- `test_timeout.ml` - Connection timeout handling

### Blink Examples (packages/blink/examples/)

Run with: `tusk run blink:example_name`

- `simple_http_test.ml` - Basic HTTP GET request
- `simple_https.ml` - HTTPS request with TLS
- `test_https_httpbin.ml` - Testing against HTTPBin API

### Std Examples (packages/std/examples/)

Run with: `tusk run std:example_name`

- `https_client.ml` - HTTP client using Std.Net
- `csv_tool.ml` - CSV parsing and processing
- `unicode_example.ml` - Unicode text handling
- `uuid_test.ml` - UUID generation

### Running Examples

```bash
# List all available examples
tusk completions --binaries | grep example

# Run specific example
tusk run suri:hello_world
tusk run suri:basic_component
tusk run blink:simple_https

# Run with arguments (if the example accepts them)
tusk run suri:json_api -- --port 3000
```

### Learning Path

**Recommended order for learning Suri:**

1. **Start Simple** - `hello_world.ml` 
   - Understand basic server setup
   
2. **Add Routing** - `routing.ml`, `router_params.ml`
   - Learn routing and parameter extraction
   
3. **Build Components** - `basic_component.ml`, `design_system.ml`
   - Create type-safe HTML UIs
   
4. **Add APIs** - `json_api.ml`
   - Build RESTful JSON APIs
   
5. **Make it Interactive** - `liveview_counter.ml`, `liveview_migration.ml`
   - Add server-side interactivity with LiveView

---

## Getting Help

### Documentation

- **Std.Net documentation**: `packages/std/src/net/*.mli`
  - Complete HTTP, TCP, TLS, URI APIs with "When to use" guidance
  
- **Blink documentation**: `packages/blink/src/*.mli`
  - HTTP/HTTPS client with connection pooling
  
- **Suri documentation**: `packages/suri/src/suri.mli`
  - Web framework overview with architecture
  - `packages/suri/src/web_server/*.mli` - Request/Response APIs
  - `packages/suri/src/middleware/*.mli` - Conn, Router, Pipeline
  - `packages/suri/src/component/component.mli` - Component system
  - `packages/suri/src/liveview/liveview.mli` - LiveView framework
  
- **Sqlx documentation**: `packages/sqlx/src/*.mli`
  - Database-agnostic SQL interface
  
- **Examples**: `packages/*/examples/` directories
  - Working code for all features
  
- **Example Documentation**: `packages/suri/EXAMPLES.md`
  - Detailed walkthrough of Suri examples

### Quick Reference Card

```ocaml
(* Std.Net.Http - HTTP primitives *)
Net.Http.Method.Get, Net.Http.Method.Post
Net.Http.Status.Ok, Net.Http.Status.Not_found
Net.Http.Request.get uri
Net.Http.Response.ok ~body:"Hello" ()

(* Suri.WebServer - HTTP server *)
WebServer.start_link ~port:8080 ~config ~handler ()
WebServer.Request.uri req, WebServer.Request.body req
WebServer.Response.ok ~body:"Success" ()
WebServer.Response.created ~headers:[...] ~body:"..." ()

(* Suri.Component - UI components *)
Component.html [head [...]; body [...]]
Component.div ~attrs:[class_ "container"] [...]
Component.button ~attrs:[on_click handler] [text "Click"]

(* Suri.Middleware - Request processing *)
Middleware.Pipeline.create () |> Pipeline.plug middleware
Middleware.Router.create routes |> Router.get "/" handler
Conn.method_ conn, Conn.uri conn, Conn.params conn

(* Blink - HTTP client *)
Blink.connect uri
Blink.request conn req ()
Blink.await conn  (* Get response *)
```

---

**Happy web programming with Riot ML!** 🌐🚀
