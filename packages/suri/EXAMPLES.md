# Suri Examples

Comprehensive examples showing how to use Suri's components together.

## Table of Contents

- [Simple HTTP Server](#simple-http-server)
- [WebSocket Echo Server](#websocket-echo-server)
- [LiveView Counter](#liveview-counter)
- [LiveView Todo List](#liveview-todo-list)
- [Full Web Application](#full-web-application)

---

## Simple HTTP Server

Basic HTTP server with routing and middleware:

```ocaml
open Std
open Suri

let home_handler conn =
  conn
  |> Middleware.Conn.with_status `OK
  |> Middleware.Conn.with_body "Welcome to Suri!"
  |> Middleware.Conn.send

let about_handler conn =
  let html = {|
    <!DOCTYPE html>
    <html>
      <body>
        <h1>About Us</h1>
        <p>Built with Suri web framework</p>
      </body>
    </html>
  |} in
  conn
  |> Middleware.Conn.with_status `OK
  |> Middleware.Conn.with_header "Content-Type" "text/html"
  |> Middleware.Conn.with_body html
  |> Middleware.Conn.send

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/about" about_handler;
  ]

let app =
  Middleware.Pipeline.[
    Middleware.Router.middleware routes;
  ]

let () =
  let config = WebServer.Config.make ~port:8080 () in
  let handler socket_conn req =
    let conn = Middleware.Conn.make socket_conn req in
    let conn = Middleware.Pipeline.run conn app in
    Middleware.Conn.to_response conn
  in
  match WebServer.start config handler with
  | Ok () -> Log.info "Server started on port 8080"
  | Error err -> Log.error "Failed to start server: %a" Net.pp_error err
```

---

## WebSocket Echo Server

WebSocket server that echoes back messages:

```ocaml
open Std
open Suri

module EchoHandler = struct
  type args = unit
  type state = { message_count : int }

  let init () = `ok { message_count = 0 }

  let handle_frame frame _conn state =
    match frame.Http.Ws.Frame.opcode with
    | Http.Ws.Frame.Text ->
        Log.info "Received: %s" frame.payload;
        let response = Http.Ws.Frame.text (Format.sprintf "Echo: %s" frame.payload) in
        `push ([response], { message_count = state.message_count + 1 })
    | Http.Ws.Frame.Ping ->
        `push ([Http.Ws.Frame.pong ()], state)
    | Http.Ws.Frame.Binary ->
        let response = Http.Ws.Frame.binary frame.payload in
        `push ([response], state)
    | _ -> `ok state

  let handle_message _msg state = `ok state
end

let ws_handler conn =
  let (upgrade_opts, handler) = (
    Channel.Handler.{ do_upgrade = true },
    Channel.Handler.make (module EchoHandler) ()
  ) in
  (* Upgrade to WebSocket *)
  (* Note: This requires WebSocket upgrade integration in WebServer *)
  conn

let routes =
  Middleware.Router.[
    get "/ws/echo" ws_handler;
  ]
```

---

## LiveView Counter

Interactive counter with server-side state:

```ocaml
open Std
open Suri

module Counter = struct
  type state = {
    count : int;
    step : int;
  }

  type msg =
    | Increment
    | Decrement
    | Reset
    | SetStep of int

  let init _conn = {
    count = 0;
    step = 1;
  }

  let update msg state =
    match msg with
    | Increment -> { state with count = state.count + state.step }
    | Decrement -> { state with count = state.count - state.step }
    | Reset -> { state with count = 0 }
    | SetStep step -> { state with step }

  let render ~state () =
    let open LiveView.Html in
    div ~id:"counter" [
      h1 [ string "Counter: "; int state.count ];
      div [
        button ~on_click:(fun _ -> Decrement) [ string "-" ] ();
        button ~on_click:(fun _ -> Reset) [ string "Reset" ] ();
        button ~on_click:(fun _ -> Increment) [ string "+" ] ();
      ] ();
      div [
        string "Step: ";
        button ~on_click:(fun _ -> SetStep 1) [ string "1" ] ();
        button ~on_click:(fun _ -> SetStep 5) [ string "5" ] ();
        button ~on_click:(fun _ -> SetStep 10) [ string "10" ] ();
      ] ()
    ] ()
end

let counter_ws conn =
  let (_opts, handler) = LiveView.mount (module Counter) in
  (* Upgrade to WebSocket with LiveView handler *)
  conn

let counter_html _conn =
  let html = {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>LiveView Counter</title>
        <style>
          body { font-family: sans-serif; padding: 20px; }
          #counter { max-width: 400px; margin: 0 auto; }
          button { margin: 5px; padding: 10px 20px; font-size: 16px; }
          h1 { text-align: center; }
        </style>
      </head>
      <body>
        <div id="counter">Loading...</div>
        <script src="/liveview/runtime.js"></script>
        <script>
          window.spawnLiveView('counter', '/ws/counter');
        </script>
      </body>
    </html>
  |} in
  Middleware.Conn.(
    conn
    |> with_status `OK
    |> with_header "Content-Type" "text/html"
    |> with_body html
    |> send
  )

let routes =
  Middleware.Router.[
    get "/counter" counter_html;
    get "/ws/counter" counter_ws;
  ]
```

---

## LiveView Todo List

Full todo list with add, remove, and filter functionality:

```ocaml
open Std
open Suri

module TodoList = struct
  type filter = All | Active | Completed

  type todo = {
    id : int;
    text : string;
    completed : bool;
  }

  type state = {
    todos : todo list;
    input : string;
    filter : filter;
    next_id : int;
  }

  type msg =
    | UpdateInput of string
    | AddTodo
    | ToggleTodo of int
    | RemoveTodo of int
    | SetFilter of filter
    | ClearCompleted

  let init _conn = {
    todos = [];
    input = "";
    filter = All;
    next_id = 0;
  }

  let update msg state =
    match msg with
    | UpdateInput text ->
        { state with input = text }
    | AddTodo when String.length state.input > 0 ->
        let todo = {
          id = state.next_id;
          text = state.input;
          completed = false;
        } in
        {
          todos = todo :: state.todos;
          input = "";
          filter = state.filter;
          next_id = state.next_id + 1;
        }
    | AddTodo -> state
    | ToggleTodo id ->
        let todos = List.map (fun todo ->
          if todo.id = id then
            { todo with completed = not todo.completed }
          else todo
        ) state.todos in
        { state with todos }
    | RemoveTodo id ->
        let todos = List.filter (fun todo -> todo.id <> id) state.todos in
        { state with todos }
    | SetFilter filter ->
        { state with filter }
    | ClearCompleted ->
        let todos = List.filter (fun todo -> not todo.completed) state.todos in
        { state with todos }

  let render_todo todo =
    let open LiveView.Html in
    let checkbox_attrs = if todo.completed then
      [("checked", "checked")]
    else [] in
    div ~attrs:[("class", "todo-item")] [
      El {
        tag = "input";
        attrs = [
          attr "type" "checkbox";
          event "change" (fun _ -> ToggleTodo todo.id)
        ] @ List.map (fun (k, v) -> `attr (k, v)) checkbox_attrs;
        children = [];
      };
      span ~children:[string todo.text] ();
      button ~on_click:(fun _ -> RemoveTodo todo.id) [
        string "✕"
      ] ()
    ] ()

  let filter_todos state =
    match state.filter with
    | All -> state.todos
    | Active -> List.filter (fun t -> not t.completed) state.todos
    | Completed -> List.filter (fun t -> t.completed) state.todos

  let render ~state () =
    let open LiveView.Html in
    let filtered = filter_todos state in
    div ~id:"todo-app" [
      h1 [ string "Todos" ];
      div ~attrs:[("class", "input-area")] [
        El {
          tag = "input";
          attrs = [
            attr "type" "text";
            attr "placeholder" "What needs to be done?";
            attr "value" state.input;
            event "input" (fun v -> UpdateInput v);
          ];
          children = [];
        };
        button ~on_click:(fun _ -> AddTodo) [ string "Add" ] ()
      ] ();
      div ~attrs:[("class", "filters")] [
        button ~on_click:(fun _ -> SetFilter All) [
          string (if state.filter = All then "[All]" else "All")
        ] ();
        button ~on_click:(fun _ -> SetFilter Active) [
          string (if state.filter = Active then "[Active]" else "Active")
        ] ();
        button ~on_click:(fun _ -> SetFilter Completed) [
          string (if state.filter = Completed then "[Completed]" else "Completed")
        ] ();
      ] ();
      div ~attrs:[("class", "todo-list")] (
        List.map render_todo filtered
      ) ();
      div ~attrs:[("class", "footer")] [
        span [ string (Format.sprintf "%d items left" (
          List.length (List.filter (fun t -> not t.completed) state.todos)
        )) ];
        button ~on_click:(fun _ -> ClearCompleted) [
          string "Clear completed"
        ] ()
      ] ()
    ] ()
end
```

---

## Full Web Application

Complete application with multiple LiveView components, routing, and static files:

```ocaml
open Std
open Suri

(* Components *)
module Counter = (* Counter from above *)
module TodoList = (* TodoList from above *)

(* Static pages *)
let home_page _conn =
  let html = {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>Suri Demo App</title>
        <link rel="stylesheet" href="/static/app.css">
      </head>
      <body>
        <nav>
          <a href="/">Home</a>
          <a href="/counter">Counter</a>
          <a href="/todos">Todos</a>
        </nav>
        <main>
          <h1>Welcome to Suri Demo</h1>
          <p>A full-featured web framework for OCaml</p>
          <ul>
            <li><a href="/counter">Interactive Counter</a></li>
            <li><a href="/todos">Todo List</a></li>
          </ul>
        </main>
      </body>
    </html>
  |} in
  Middleware.Conn.(
    conn
    |> with_status `OK
    |> with_header "Content-Type" "text/html"
    |> with_body html
    |> send
  )

(* LiveView pages *)
let counter_page _conn =
  (* Serve HTML shell for counter LiveView *)
  let html = {|
    <!DOCTYPE html>
    <html>
      <head>
        <title>Counter</title>
        <link rel="stylesheet" href="/static/app.css">
      </head>
      <body>
        <nav><a href="/">Home</a></nav>
        <div id="counter">Loading...</div>
        <script src="/liveview/runtime.js"></script>
        <script>
          window.spawnLiveView('counter', '/ws/counter');
        </script>
      </body>
    </html>
  |} in
  Middleware.Conn.(
    conn
    |> with_status `OK
    |> with_header "Content-Type" "text/html"
    |> with_body html
    |> send
  )

let todos_page _conn =
  (* Similar to counter_page but for todos *)
  ...

(* WebSocket handlers *)
let counter_ws conn =
  let (_opts, handler) = LiveView.mount (module Counter) in
  (* Upgrade to WebSocket *)
  conn

let todos_ws conn =
  let (_opts, handler) = LiveView.mount (module TodoList) in
  (* Upgrade to WebSocket *)
  conn

(* Routes *)
let routes =
  Middleware.Router.[
    (* Static pages *)
    get "/" home_page;
    
    (* LiveView pages *)
    get "/counter" counter_page;
    get "/todos" todos_page;
    
    (* WebSocket endpoints *)
    get "/ws/counter" counter_ws;
    get "/ws/todos" todos_ws;
  ]

(* Middleware pipeline *)
let app =
  Middleware.Pipeline.[
    (* Add logging, CORS, sessions, etc. here *)
    Middleware.Router.middleware routes;
  ]

(* Server *)
let () =
  let config = WebServer.Config.make ~port:8080 () in
  let handler socket_conn req =
    let conn = Middleware.Conn.make socket_conn req in
    let conn = Middleware.Pipeline.run conn app in
    Middleware.Conn.to_response conn
  in
  match WebServer.start config handler with
  | Ok () ->
      Log.info "Server started on http://localhost:8080";
      Miniriot.wait_forever ()
  | Error err ->
      Log.error "Server failed: %a" Net.pp_error err;
      exit 1
```

---

## Running the Examples

To run these examples:

1. Save the code to a file (e.g., `app.ml`)
2. Create a `tusk.toml` that depends on `suri`
3. Build and run:
   ```bash
   tusk build
   tusk run app
   ```
4. Open http://localhost:8080 in your browser

## Next Steps

- Add authentication/authorization middleware
- Integrate database for persistent storage
- Add CSRF protection
- Implement session management
- Add static file serving
- Deploy with systemd or Docker
