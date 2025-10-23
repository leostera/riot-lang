# Tusk Protocol API

Clean, simple API for Tusk's protocol definitions.

## Module Hierarchy

```
Tusk_protocol
│
├── Internal Protocol (typed, for server internals)
│   ├── type target = All | Package of string
│   ├── type request (with Pid.t, Path.t, etc.)
│   ├── type response (with Workspace.t, Package.t, etc.)
│   └── module BuildStats
│
└── Wire (module - for RPC)
    ├── method_ping : string
    ├── method_build_all : string
    ├── ... (other method constants)
    │
    └── WireProtocol (module)
        ├── type request (JSON-serializable)
        ├── type response (JSON-serializable)
        └── implements Jsonrpc.ApplicationProtocol
```

## Key Design Principles

1. **Typed Internal Protocol**: `Tusk_protocol.*` uses rich OCaml types for type safety
2. **Simple Wire Protocol**: `Tusk_protocol.Wire.*` uses only JSON-serializable types
3. **Clear Separation**: Protocol conversion happens at the RPC boundary
4. **Short Paths**: `Wire` is easier to type than `Jsonrpc_protocol`

## Usage Examples

### RPC Client

```ocaml
open Tusk_protocol

(* Use wire protocol for network communication *)
module WireProtocol = Wire.WireProtocol

let client = 
  Jsonrpc.Client.create
    ~protocol:(module WireProtocol)
    ~transport:(module Std.Net.TcpClient)
    transport

(* Make requests *)
match Jsonrpc.Client.call client 
    ~method_:Wire.method_ping 
    ~params:NoParams () with
| Ok WireProtocol.Pong -> print_endline "Server is alive!"
| Error e -> print_endline "Error!"
```

### RPC Server

```ocaml
open Tusk_protocol
open Miniriot

(* Handle wire protocol requests *)
let handle_wire_request wire_req client_pid = 
  (* Convert to internal protocol *)
  let internal_req = match wire_req with
    | Wire.WireProtocol.Ping -> 
        Ping { client_pid }
    | Wire.WireProtocol.BuildAll -> 
        Build { client_pid; target = All; session_id = Session_id.make () }
    | Wire.WireProtocol.BuildPackage pkg ->
        Build { client_pid; target = Package pkg; session_id = Session_id.make () }
    (* ... *)
  in
  
  (* Send to server using typed protocol *)
  send server_pid (ServerRequest internal_req);
  
  (* Receive typed response *)
  match receive ~selector:(function 
    | ServerResponse resp -> `select resp 
    | _ -> `skip) () with
  | Pong -> Wire.WireProtocol.Pong
  | BuildStarted { session_id; started_at } ->
      Wire.WireProtocol.BuildStarted { session_id; started_at }
  (* ... *)
```

### Internal Server Logic

```ocaml
open Tusk_protocol

(* Work with rich, typed protocol *)
let handle_internal_request = function
  | Build { client_pid; target; session_id } ->
      let packages = match target with
        | All -> workspace.packages
        | Package name -> find_package workspace name
      in
      (* ... do the build ... *)
      send client_pid (ServerResponse (BuildStarted { session_id; started_at }))
      
  | Ping { client_pid } ->
      send client_pid (ServerResponse Pong)
      
  | GetWorkspaceConfig { client_pid } ->
      send client_pid (ServerResponse (WorkspaceConfig { 
        workspace; 
        toolchain 
      }))
```

## Type Safety Benefits

The split between internal and wire protocols provides:

1. **Compile-time safety** for server internals (can't forget client_pid)
2. **Simple serialization** for network communication (no complex types)
3. **Clear boundaries** between network and application layers
4. **Type-driven development** (compiler helps guide conversions)

## Migration Guide

### Old Code
```ocaml
open Tusk_jsonrpc

module WireProtocol = WireProtocol
let req = WireProtocol.BuildAll
```

### New Code
```ocaml
open Tusk_protocol

module WireProtocol = Wire.WireProtocol
let req = WireProtocol.BuildAll
```

Simple one-word change: `Jsonrpc_protocol` → `Wire`
