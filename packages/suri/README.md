# Suri - Web Framework for OCaml

Suri is an experimental, supervised web framework built on Std and Actors for building HTTP servers, WebSocket servers, and real-time web applications.

Suri is not production-ready yet. The core framework shape is usable for experiments, examples, and hardening work, but protocol correctness, cryptography, operational limits, and production guidance are still under active development.

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

### LiveView (Experimental)
Server-rendered components with live DOM updates over WebSocket. LiveView exists, but its token handling, lifecycle cleanup, reconnect behavior, and event authorization are still being hardened.

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
- **Lifecycle Hooks**: Handler callbacks for connection close, errors, and shutdown
- **Monitoring**: Track acceptor health and restart patterns

## Components

1. **SocketPool** - Supervised TCP connection management
2. **WebServer** - HTTP protocol handling (HTTP/1.1 foundation; HTTP/2 prototype)
3. **Middleware** - Request processing pipeline
4. **Channel** - WebSocket communication layer
5. **LiveView** - Experimental real-time UI components

This design allows you to use just the components you need, from low-level socket handling to high-level web frameworks.

## Features

### Current
- HTTP/1.1 server foundation with keep-alive support
- OTP-style supervision with `Std.Supervisor`
- Concurrent connection acceptance with configurable acceptor count
- WebSocket upgrade and Channel primitives
- Composable middleware framework
- Request routing
- Component rendering with escaped text and attributes

### Experimental / Hardening In Progress
- LiveView server-side rendering and WebSocket updates
- HTTP/2 protocol handling
- Session and CSRF middleware
- Static file serving
- Request body parsing and upload handling
- Operational limits, timeouts, graceful shutdown, and production presets

## Examples

See [packages/suri/examples](./examples) for examples including:
- Simple HTTP server
- JSON API server
- WebSocket echo server
- Middleware and routing
- LiveView components

## Performance

Suri is designed for high concurrency:
- Default 100 concurrent acceptors
- Each connection handled in its own process
- Non-blocking I/O
- Automatic acceptor restart on crash
- Configurable buffer sizes and limits
