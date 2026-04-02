# Suri - Web Framework for OCaml

Suri is a high-performance, supervised web framework built on Std and Actors for building HTTP servers, WebSocket servers, and real-time web applications.

## Quick Start

```ocaml
open Std
open Suri

let handler _conn req =
  let open WebServer in
  match Request.path req with
  | "/" -> Response.ok ~body:"Hello from Suri!" ()
  | _ -> Response.not_found ~body:"Not Found" ()

let main () =
  let config = WebServer.Config.make () in
  match WebServer.start_link ~port:8080 ~config ~handler () with
  | Ok supervisor ->
      Log.info "Server running on http://0.0.0.0:8080";
      receive_any ()  (* Keep alive *)
  | Error `Bind_error ->
      Log.error "Failed to bind to port";
      Error (Failure "bind error")

let () = run_with @@ main
```

## Components

### SocketPool
Supervised TCP connection pool with concurrent acceptors and protocol switching. Uses `Std.Supervisor.Dynamic` to manage acceptor processes with automatic restart on failure.

### WebServer
HTTP/1.1 server with request parsing, response handling, and keep-alive connection management. Built on SocketPool with full supervision support.

### Middleware
Composable middleware framework with routing support for building HTTP request pipelines.

### Channel
WebSocket handler abstraction for real-time bidirectional communication. Provides Frame types and Handler interface for building WebSocket servers.

### LiveView (Coming Soon)
Server-rendered components with live DOM updates over WebSocket. Build interactive UIs with server-side state management and automatic client updates.

## Architecture

Suri follows a supervised, layered architecture using OTP-style process supervision:

```
Application Supervisor
    ↓
WebServer (Supervisor.Dynamic)
    ↓
Acceptor Pool (100+ concurrent processes)
    ↓
Connection Handlers (1 per connection)
    ↓
Your Application Logic
```

### Supervision Benefits

- **Fault Tolerance**: If an acceptor crashes, it's automatically restarted
- **Resource Management**: Bounded number of acceptors with configurable limits
- **Graceful Shutdown**: Proper cleanup of resources on termination
- **Monitoring**: Track acceptor health and restart patterns

## Components

1. **SocketPool** - Supervised TCP connection management
2. **WebServer** - HTTP protocol handling (HTTP/1.1, HTTP/2 planned)
3. **Middleware** - Request processing pipeline
4. **Channel** - WebSocket communication layer
5. **LiveView** - Real-time UI components (coming soon)

This design allows you to use just the components you need, from low-level socket handling to high-level web frameworks.

## Features

### Current
- ✅ HTTP/1.1 server with keep-alive support
- ✅ OTP-style supervision with `Std.Supervisor`
- ✅ Concurrent connection acceptance (configurable pool size)
- ✅ WebSocket support via Channel
- ✅ Composable middleware framework
- ✅ Request routing

### Planned
- 🚧 LiveView with server-side rendering
- 🚧 HTTP/2 support
- 🚧 Session management
- 🚧 CSRF protection
- 🚧 Authentication/authorization middleware
- 🚧 Static file serving
- 🚧 Database integration helpers

## Examples

See [EXAMPLES.md](./EXAMPLES.md) for comprehensive examples including:
- Simple HTTP server
- JSON API server
- WebSocket echo server
- Middleware and routing
- LiveView components (coming soon)

## Performance

Suri is designed for high concurrency:
- Default 100 concurrent acceptors
- Each connection handled in its own process
- Non-blocking I/O
- Automatic acceptor restart on crash
- Configurable buffer sizes and limits
