# Suri Examples

Comprehensive examples showing how to use Suri with proper supervision.

## Table of Contents

- [Hello World](#hello-world)
- [Simple HTTP Server](#simple-http-server)
- [JSON API Server](#json-api-server)
- [WebSocket Echo Server](#websocket-echo-server)
- [Middleware and Routing](#middleware-and-routing)
- [Type-Safe HTML Components](#type-safe-html-components)

---

## Hello World

The simplest possible Suri server:

```ocaml
open Std
open Suri

let handler _conn _req =
  WebServer.Response.ok ~body:"Hello, World!" ()

let main () =
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok supervisor ->
      Log.info "Server running on http://0.0.0.0:8080";
      receive_any ()  (* Keep process alive *)
  | Error `Bind_error ->
      Log.error "Failed to bind to port 8080";
      Error (Failure "bind error")

let () = run_with @@ main
```

**Run it:**
```bash
tusk run hello_world
curl http://localhost:8080
# Output: Hello, World!
```

---

## Simple HTTP Server

HTTP server with basic routing:

```ocaml
open Std
open Suri

let handler _conn req =
  let open WebServer in
  let path = Request.path req in
  let method_ = Request.method_ req in
  
  Log.info "%s %s" (Http.Method.to_string method_) path;
  
  match (method_, path) with
  | (GET, "/") ->
      Response.ok ~body:"Welcome to Suri!" ()
  
  | (GET, "/about") ->
      let html = {|
<!DOCTYPE html>
<html>
  <head><title>About</title></head>
  <body>
    <h1>About Suri</h1>
    <p>A high-performance web framework for OCaml</p>
    <a href="/">Home</a>
  </body>
</html>
      |} in
      Response.ok
        ~headers:(Http.Header.of_list [("Content-Type", "text/html")])
        ~body:html
        ()
  
  | (GET, "/health") ->
      Response.ok ~body:"OK" ()
  
  | _ ->
      Response.not_found ~body:"404 - Not Found" ()

let main () =
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok supervisor ->
      Log.info "Server started on http://0.0.0.0:8080";
      Log.info "Try: curl http://localhost:8080/";
      Log.info "     curl http://localhost:8080/about";
      receive_any ()
  | Error `Bind_error ->
      Error (Failure "Failed to bind")

let () = run_with @@ main
```

---

## JSON API Server

RESTful JSON API using `Data.Json` for proper JSON handling:

```ocaml
open Std
open Suri

type user = {
  id : int;
  name : string;
  email : string;
}

(* In-memory database *)
let users = [
  { id = 1; name = "Alice"; email = "alice@example.com" };
  { id = 2; name = "Bob"; email = "bob@example.com" };
]

(* Convert user to JSON using Data.Json *)
let user_to_json user =
  Data.Json.obj [
    ("id", Data.Json.int user.id);
    ("name", Data.Json.string user.name);
    ("email", Data.Json.string user.email);
  ]

let users_to_json users =
  Data.Json.array (List.map user_to_json users)

let json_headers = [
  ("Content-Type", "application/json");
  ("Access-Control-Allow-Origin", "*");
]

let json_response json =
  WebServer.Response.ok
    ~headers:json_headers
    ~body:(Data.Json.to_string json)
    ()

let handler _conn req =
  let open WebServer in
  let uri = Request.uri req in
  
  if uri = "/api/users" then
    json_response (users_to_json users)
  else if String.starts_with ~prefix:"/api/users/" uri then
    let id_str = String.sub uri 11 (String.length uri - 11) in
    (try
      let id = Int.of_string id_str in
      match List.find_opt (fun u -> u.id = id) users with
      | Some user -> json_response (user_to_json user)
      | None -> 
          let error = Data.Json.obj [("error", Data.Json.string "User not found")] in
          Response.not_found ~headers:json_headers 
            ~body:(Data.Json.to_string error) ()
    with Failure _ -> 
      let error = Data.Json.obj [("error", Data.Json.string "Invalid user ID")] in
      Response.bad_request ~headers:json_headers 
        ~body:(Data.Json.to_string error) ())
  else if uri = "/" then
    let info = Data.Json.obj [
      ("api", Data.Json.string "JSON API Example");
      ("endpoints", Data.Json.obj [
        ("/api/users", Data.Json.string "List all users");
        ("/api/users/:id", Data.Json.string "Get user by ID");
      ])
    ] in
    json_response info
  else
    let error = Data.Json.obj [("error", Data.Json.string "Endpoint not found")] in
    Response.not_found ~headers:json_headers 
      ~body:(Data.Json.to_string error) ()

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    let config = WebServer.Config.make () in
    let supervisor = match WebServer.start_link ~port:8080 ~config ~handler () with
      | Ok s -> s
      | Error `Bind_error -> panic "Failed to bind to port"
    in
    
    Log.info "JSON API server on http://0.0.0.0:8080";
    Log.info "Try: curl http://localhost:8080/api/users";
    
    let _ = receive_any () in
    Ok ()
  )
```

**Test it:**
```bash
curl http://localhost:8080/api/users
# [{"id":1,"name":"Alice","email":"alice@example.com"},...]

curl http://localhost:8080/api/users/1
# {"id":1,"name":"Alice","email":"alice@example.com"}

curl http://localhost:8080/api/users/999
# {"error":"User not found"}
```

**Benefits of using `Data.Json`:**
- ✅ Type-safe JSON construction
- ✅ Automatic escaping and formatting
- ✅ No manual string concatenation
- ✅ Easy to compose and nest objects
- ✅ Can parse incoming JSON with `Data.Json.of_string`

---

## WebSocket Echo Server

WebSocket server that echoes back messages (Channel API):

```ocaml
open Std
open Suri

(* WebSocket support coming soon! *)
(* For now, use SocketPool.Handler directly for custom protocols *)

module EchoProtocol = struct
  include SocketPool.Handler.Default
  
  type state = { message_count : int }
  type error = string

  let handle_connection _conn state =
    Log.info "WebSocket-like connection established";
    Continue { message_count = 0 }

  let handle_data data conn state =
    Log.info "Received %d bytes" (String.length data);
    match SocketPool.Connection.send conn ("Echo: " ^ data) with
    | Ok () -> 
        Continue { message_count = state.message_count + 1 }
    | Error `Closed -> 
        Close state

  let handle_close _conn state =
    Log.info "Connection closed after %d messages" state.message_count
end

let main () =
  match SocketPool.start_link
    ~host:"0.0.0.0"
    ~port:9000
    ~acceptors:50
    (module EchoProtocol)
    { EchoProtocol.message_count = 0 }
  with
  | Ok supervisor ->
      Log.info "Echo server on port 9000";
      Log.info "Try: telnet localhost 9000";
      receive_any ()
  | Error `Bind_error ->
      Error (Failure "Failed to bind")

let () = run_with @@ main
```

**Test it:**
```bash
telnet localhost 9000
# Type anything and see it echoed back
```

---

## Middleware and Routing

Using middleware for logging and routing (coming soon):

```ocaml
(* Middleware support is being redesigned to work with the new supervision model.
   For now, implement routing directly in your handler function. *)

open Std
open Suri

let handler _conn req =
  let open WebServer in
  let path = Request.path req in
  let method_ = Request.method_ req in
  
  (* Simple logging *)
  Log.info "%s %s" (Http.Method.to_string method_) path;
  
  (* Routing *)
  match (method_, path) with
  | (GET, "/") -> Response.ok ~body:"Home" ()
  | (GET, "/api/status") ->
      Response.ok
        ~headers:(Http.Header.of_list [("Content-Type", "application/json")])
        ~body:{|{"status":"ok"}|}
        ()
  | (POST, "/api/echo") ->
      let body = Request.body req in
      Response.ok ~body ()
  | _ -> Response.not_found ~body:"Not Found" ()

let main () =
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok _supervisor ->
      Log.info "Server with routing on http://0.0.0.0:8080";
      receive_any ()
  | Error `Bind_error ->
      Error (Failure "Failed to bind")

let () = run_with @@ main
```

---

## Running the Examples

### 1. Save the example code
Save any example to a file in your project, e.g., `examples/hello_world.ml`

### 2. Update tusk.toml
```toml
[package]
name = "my_app"

[dependencies]
std = { path = "../std" }
suri = { path = "../suri" }

[[bin]]
name = "hello_world"
path = "examples/hello_world.ml"
```

### 3. Build and run
```bash
tusk build
tusk run hello_world
```

### 4. Test it
```bash
curl http://localhost:8080
```

## Performance Tips

1. **Tune acceptors**: Increase `~acceptors` for high-load scenarios
   ```ocaml
   WebServer.start_link ~port:8080 ~acceptors:200 ~config ~handler ()
   ```

2. **Adjust buffer size**: Larger buffers for big requests
   ```ocaml
   let config = WebServer.Config.make ~buffer_size:8192 ()
   ```

3. **Monitor supervisor**: Use `Supervisor.Dynamic.count_children` to monitor health
   ```ocaml
   let count = Supervisor.Dynamic.count_children supervisor in
   Log.info "Active acceptors: %d" count.active
   ```

---

## Type-Safe HTML Components

Build type-safe, composable HTML with `Suri.Component`:

```ocaml
open Std
open Suri
open Suri.Component

(* Simple component - works for static HTML and LiveView *)
let welcome_page : unit t =
  html [
    head [
      title_ [text "Welcome"];
      meta ~attrs:[attr "charset" "UTF-8"] ();
    ];
    body [
      div ~attrs:[class_ "container"] [
        h1 [text "Welcome to Suri Components"];
        p [text "Build type-safe HTML with OCaml"];
        
        (* Form with proper attributes *)
        form ~attrs:[action "/submit"; method_ "POST"] [
          div ~attrs:[class_ "form-group"] [
            label ~attrs:[for_ "email"] [text "Email"];
            input ~attrs:[
              type_ "email";
              id "email";
              name "email";
              placeholder "you@example.com";
              required;
            ] ();
          ];
          button ~attrs:[type_ "submit"; class_ "btn"] [
            text "Submit"
          ];
        ];
      ];
    ];
  ]

(* Render to HTML string *)
let () =
  let html = to_html welcome_page in
  println html
```

**Run it:**
```bash
tusk run suri:basic_component
```

### Reusable Design System

Create a library of reusable components:

```ocaml
open Std
open Suri
open Suri.Component

module MyDesign = struct
  (* Design tokens *)
  let primary_color = "#007bff"
  let spacing_md = "16px"
  
  (* Layout components *)
  let container ?(max_width = "1200px") children =
    div ~attrs:[
      class_ "container";
      style ("max-width: " ^ max_width ^ "; margin: 0 auto; padding: 0 " ^ spacing_md);
    ] children
  
  let card children =
    div ~attrs:[
      class_ "card";
      style "border: 1px solid #e0e0e0; border-radius: 8px; padding: 24px; background: white;";
    ] children
  
  let button_primary children =
    button ~attrs:[
      class_ "btn btn-primary";
      style ("background: " ^ primary_color ^ "; color: white; border: none; padding: 10px 20px;");
    ] children
  
  let badge ~variant content =
    let bg_color = match variant with
      | "success" -> "#28a745"
      | "danger" -> "#dc3545"
      | _ -> primary_color
    in
    span ~attrs:[
      class_ ("badge badge-" ^ variant);
      style ("background: " ^ bg_color ^ "; color: white; padding: 4px 10px; border-radius: 12px;");
    ] [text content]
end

(* Use your design system *)
let product_card ~name ~price ~in_stock =
  MyDesign.card [
    h3 [text name];
    div [
      MyDesign.badge ~variant:(if in_stock then "success" else "danger")
        (if in_stock then "In Stock" else "Out of Stock");
    ];
    div ~attrs:[style "font-size: 24px; font-weight: bold"] [
      text ("$" ^ Float.to_string price)
    ];
    MyDesign.button_primary [text "Add to Cart"];
  ]
```

**Run it:**
```bash
tusk run suri:design_system
```

### Progressive Enhancement: Static → LiveView

The same components work for both static HTML and interactive LiveView:

```ocaml
open Std
open Suri
open Suri.Component

(* Define message type for LiveView *)
type msg = Increment | Decrement | Reset

(* Step 1: Static component (no interactivity) *)
let counter_static count =
  div ~attrs:[class_ "counter"] [
    h1 [text "Counter"];
    div ~attrs:[class_ "count"] [text (Int.to_string count)];
    div [
      button [text "-"];  (* Not interactive in static HTML *)
      button [text "+"];
    ];
  ]

(* Step 2: Add event handlers for LiveView *)
let counter_interactive count : msg t =
  div ~attrs:[class_ "counter"] [
    h1 [text "Counter"];
    div ~attrs:[class_ "count"] [text (Int.to_string count)];
    div [
      button ~attrs:[
        on_click (fun _ -> Decrement)  (* 👈 Add handler! *)
      ] [text "-"];
      button ~attrs:[
        on_click (fun _ -> Increment)  (* 👈 Add handler! *)
      ] [text "+"];
    ];
  ]

(* Event handlers are ignored in static HTML, active in LiveView *)
let static_html = to_html (counter_static 0)

(* In LiveView, handlers are wired to your update function *)
let interactive_view = counter_interactive 42
```

**Run it:**
```bash
tusk run suri:liveview_migration
```

### Component Features

- **115+ HTML5 elements**: Complete coverage including:
  - Document structure: `html`, `head`, `body`, `base`, `meta`, `link`, etc.
  - Content sectioning: `header`, `nav`, `main`, `section`, `article`, `aside`, `footer`, `hgroup`, `search`
  - Text content: `div`, `p`, `span`, `h1`-`h6`, `pre`, `blockquote`, `figure`, `figcaption`, `menu`
  - Inline text: `a`, `abbr`, `b`, `cite`, `code`, `em`, `i`, `kbd`, `mark`, `q`, `strong`, `time`, `var`, etc.
  - Lists: `ul`, `ol`, `li`, `dl`, `dt`, `dd`
  - Tables: `table`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td`, `caption`, `col`, `colgroup`
  - Forms: `form`, `input`, `button`, `select`, `textarea`, `datalist`, `optgroup`, `output`, `progress`, `meter`
  - Interactive: `details`, `summary`, `dialog`
  - Multimedia: `img`, `audio`, `video`, `picture`, `area`, `map`, `track`, `source`
  - Embedded: `iframe`, `embed`, `object`, `canvas`, `svg`, `math`
  - Web Components: `slot`, `template`
  - Scripting: `script`, `noscript`
- **30+ attribute helpers**: `class_`, `style`, `id`, `href`, `src`, `type_`, `name`, `value`, etc.
- **15+ event handlers**: `on_click`, `on_submit`, `on_input`, `on_change`, `on_focus`, etc. (LiveView only)
- **Conditional rendering**: `when_`, `unless`, `maybe`
- **Self-closing tags**: Automatically handled (`<input />`, `<br />`, `<img />`, etc.)
- **HTML escaping**: Attributes are properly escaped
- **Type-safe**: Catch errors at compile time
- **Composable**: Build reusable component libraries

### Benefits

- ✅ Write once, render anywhere (static HTML or LiveView)
- ✅ No inline JavaScript required
- ✅ React-style component composition
- ✅ Type-safe all the way
- ✅ Easy to preview statically during development
- ✅ Add interactivity incrementally

---

## Next Steps

- ✅ Supervision and fault tolerance
- ✅ Type-safe HTML components
- 🚧 LiveView for interactive UIs
- 🚧 Middleware framework refactor
- 🚧 WebSocket support via Channel
- 🚧 Session management
- 🚧 Static file serving
- 🚧 Database integration helpers
