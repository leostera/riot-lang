# tusk-client

JSON-RPC client for connecting to and communicating with the Tusk build server.

## Features

- TCP-based connection to Tusk server
- Streaming build events via callbacks
- Full support for all Tusk RPC methods

## API

Main type: `Tusk_client.t` - Client handle

### Connection

```ocaml
val create : host:string -> port:int -> (t, string) result
val close : t -> unit
```

### Build Operations

```ocaml
type build_request = BuildPackage of string | BuildAll

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Event.t  
  | BuildFinished of (unit, string) result

val build_streaming : 
  t -> 
  build_request -> 
  (streaming_event -> unit) -> 
  (streaming_event, string) result
```

### Queries

```ocaml
val ping : t -> (unit, string) result
val get_workspace_config : t -> (WireProtocol.workspace_config, string) result
val get_build_graph : t -> (WireProtocol.build_graph_response, string) result
val get_package_info : t -> string -> (WireProtocol.package_detail, string) result
val find_executable : t -> string -> ((string * string) option, string) result
val find_artifact : t -> package:string -> kind:string -> name:string -> (string, string) result
```

### Operations

```ocaml
val format_file : t -> file_path:string -> check_only:bool -> (string * bool, string) result
val format_code : t -> code:string -> file_path:string option -> (string * bool, string) result
val format_all : t -> mode:[ `check | `write ] -> (int * int * (string * string) list, string) result
val new_package : t -> path:string -> name:string -> is_library:bool -> (string * string, string) result
val restart : t -> (unit, string) result
val shutdown : t -> (unit, string) result
```

## Dependencies

- `std` - Standard library
- `miniriot` - Actor runtime
- `tusk-model` - Core data models
- `tusk-protocol` - Protocol definitions
- `jsonrpc` - JSON-RPC implementation

## Usage

```ocaml
open Tusk_client

let client = 
  create ~host:"127.0.0.1" ~port:9001
  |> Result.expect ~msg:"Failed to connect"

let () = 
  build_streaming client BuildAll (function
    | BuildStarted sid -> print_endline "Build started"
    | BuildEvent evt -> Event.print evt
    | BuildFinished (Ok ()) -> print_endline "Build succeeded"
    | BuildFinished (Error e) -> print_endline ("Build failed: " ^ e))
  |> ignore

let () = close client
```

### Accessing Wire Protocol Types

The client re-exports `WireProtocol` from `Tusk_protocol.Wire` for convenience:

```ocaml
(* These are equivalent *)
module WireProtocol = Tusk_client.WireProtocol
module WireProtocol = Tusk_protocol.Wire.WireProtocol
```
