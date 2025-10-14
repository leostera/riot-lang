# Suri - Web Framework for OCaml

Suri is a high-performance web framework built on Std and Miniriot for building HTTP servers, WebSocket servers, and real-time web applications.

## Components

### SocketPool
TCP connection pool with concurrent acceptors and protocol switching. Provides low-level socket management.

### WebServer
HTTP/1.1 server with request parsing and response handling built on SocketPool.

### Middleware
Composable middleware framework with routing support for building HTTP request pipelines.

### Channel
WebSocket handler abstraction for real-time bidirectional communication. Provides Frame types and Handler interface for building WebSocket servers.

### LiveView
Server-rendered components with live DOM updates over WebSocket. Build interactive UIs with server-side state management and automatic client updates.

## LiveView Example

```ocaml
open Std
open Suri

(* Define a counter component *)
module Counter = struct
  type state = { count : int }
  type msg = Increment | Decrement

  let init _conn = { count = 0 }

  let update msg state =
    match msg with
    | Increment -> { count = state.count + 1 }
    | Decrement -> { count = state.count - 1 }

  let render ~state () =
    let open LiveView.Html in
    div ~id:"counter" [
      h1 [ string "Counter: "; int state.count ];
      button ~on_click:(fun _ -> Increment) [ string "+" ] ();
      button ~on_click:(fun _ -> Decrement) [ string "-" ] ();
    ] ()
end

(* Mount the LiveView in your web server *)
let () =
  let (upgrade_opts, handler) = LiveView.mount (module Counter) in
  (* Use handler with WebSocket upgrade in your HTTP server *)
  ...
```

## Architecture

Suri follows a layered architecture:

1. **SocketPool** - Low-level TCP connection management
2. **WebServer** - HTTP protocol handling (HTTP/1.1, HTTP/2 planned)
3. **Middleware** - Request processing pipeline
4. **Channel** - WebSocket communication layer
5. **LiveView** - Real-time UI components

This design allows you to use just the components you need, from low-level socket handling to high-level LiveView components.

## Features

### Current
- HTTP/1.1 server with streaming support
- WebSocket support via Channel
- LiveView with server-side rendering
- Composable middleware
- TCP connection pooling

### Planned
- HTTP/2 support
- Session management
- CSRF protection
- Authentication/authorization middleware
- Static file serving
- Database integration
- Background jobs
- Caching layer

## JavaScript Runtime

LiveView requires a small JavaScript runtime on the client side to handle WebSocket communication and DOM updates. Include it in your HTML:

```html
<script src="/liveview/runtime.js"></script>
<script>
  window.spawnLiveView('counter', '/ws/counter');
</script>
```

The runtime automatically:
- Establishes WebSocket connection
- Sends user events to server
- Applies DOM patches from server
- Handles reconnection on disconnect
- Rebinds event handlers after updates
