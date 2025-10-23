# Tusk Server Protocol

The tusk-server package contains the internal typed protocol used for message passing between server components.

## Internal Protocol

Located in `src/protocol.ml`:

```ocaml
module Protocol : sig
  type target = All | Package of string

  module BuildStats : sig
    type t
    val make : unit -> t
    val mark_started : t -> unit
    val get_build_duration : t -> float
    (* ... *)
  end

  type request =
    | Build of { client_pid : Pid.t; target : target; session_id : Session_id.t }
    | Ping of { client_pid : Pid.t }
    | GetWorkspaceConfig of { client_pid : Pid.t }
    (* ... rich types with Pid.t, Path.t, etc. *)

  type response =
    | Pong
    | BuildStarted of { session_id : Session_id.t; started_at : Datetime.t }
    | WorkspaceConfig of { workspace : Workspace.t; toolchain : Tusk_toolchain.t }
    (* ... rich types with Workspace.t, Package.t, etc. *)

  type Message.t += 
    | ServerRequest of request 
    | ServerResponse of response
end
```

## Wire Protocol Conversion

The server converts between:
- **Wire protocol** (`Tusk_protocol.Wire.WireProtocol`) - Simple JSON-serializable types
- **Internal protocol** (`Protocol`) - Rich OCaml types with Pid.t, Path.t, etc.

This conversion happens at the RPC boundary in the RPC handler.

### Example Conversion

```ocaml
(* Incoming wire request *)
let wire_req = Tusk_protocol.Wire.WireProtocol.BuildAll

(* Convert to internal protocol *)
let internal_req = Protocol.Build {
  client_pid = self ();
  target = Protocol.All;
  session_id = Session_id.make ();
}

(* Process internally *)
send server_pid (Protocol.ServerRequest internal_req)

(* Receive internal response *)
match receive () with
| Protocol.ServerResponse Protocol.Pong ->
    (* Convert to wire response *)
    Tusk_protocol.Wire.WireProtocol.Pong
```

## Why This Separation?

1. **Type Safety**: Internal code uses strong types (Pid.t, Path.t, Workspace.t)
2. **Clear Contract**: Wire protocol is the public API, internal can change freely
3. **Proper Layering**: Protocol conversion is explicit at RPC boundary
4. **Encapsulation**: Server implementation details don't leak to clients
