# tusk-protocol

Wire protocol for the Tusk build system JSON-RPC API.

## Public API

The package exposes only the wire protocol - simple, JSON-serializable types for external RPC clients:

```ocaml
module Tusk_protocol : sig
  module Wire : sig
    val method_ping : string
    val method_build_all : string
    val method_build_package : string
    (* ... method name constants *)

    module WireProtocol : sig
      type request =
        | Ping
        | BuildAll
        | BuildPackage of string
        | GetWorkspaceConfig
        | FormatFile of { file_path : string; check_only : bool }
        (* ... simple, JSON-serializable types *)

      type response =
        | Pong
        | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
        | BuildEvent of { session_id : Session_id.t; event : Event.t }
        | WorkspaceConfig of { workspace_root : string; packages : ... }
        (* ... *)

      (* Implements Jsonrpc.ApplicationProtocol *)
      val request_to_params : request -> Jsonrpc.request_params
      val request_of_params : string -> Jsonrpc.params -> (request, Json.t) result
      val response_to_json : response -> Json.t
      val response_of_json : Json.t -> (response, Json.t) result
    end
  end
end
```

## Design Philosophy

**tusk-protocol contains ONLY the wire protocol.** The internal server protocol (with `Pid.t`, `Path.t`, etc.) lives in `tusk-server` as an implementation detail.

This separation ensures:
- **Clear contract**: The wire protocol is the public API contract
- **Implementation freedom**: Server internals can change without affecting clients
- **Type safety**: Rich types in server, simple types on the wire
- **Proper layering**: Protocol conversion happens at the RPC boundary in the server

## Dependencies

- `std` - Standard library
- `miniriot` - Actor runtime  
- `tusk-model` - Core data models
- `jsonrpc` - JSON-RPC implementation

## Usage

### For RPC Clients

```ocaml
open Tusk_protocol

module WireProtocol = Wire.WireProtocol

(* Create JSON-RPC client *)
let client = 
  Jsonrpc.Client.create 
    ~protocol:(module WireProtocol)
    ~transport:(module Std.Net.TcpClient)
    transport

(* Make RPC calls *)
let response = 
  Jsonrpc.Client.call client 
    ~method_:Wire.method_ping 
    ~params:NoParams ()
```

### For Servers

The server uses its own internal protocol and converts to/from wire protocol at the RPC boundary.

See `tusk-server` for the internal protocol implementation and conversion logic.

## Used By

- `tusk-client` - RPC client implementation
- `tusk-server` - RPC server and internal protocol handling  
- `tusk-cli` - CLI commands using protocol types
