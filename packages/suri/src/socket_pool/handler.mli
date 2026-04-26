(**
   Connection handler abstraction with protocol switching support.

   Handlers define how connections are processed through lifecycle hooks. The
   key feature is the ability to switch handlers mid-connection via the
   [Switch] result, enabling protocol upgrades (e.g., HTTP → WebSocket).
*)
open Std

type ('state, 'error) handler_result =
  | Ok
  | Continue of 'state
  | Close of 'state
  | Error of 'state * 'error
  | Switch of t

and ('state, 'error) handler = {
  to_string_error: 'error -> string;
  (** Convert 'error to string for logging *)
  handle_close: Connection.t -> 'state -> unit;
  (** Called when connection is closing for cleanup *)
  handle_connection: Connection.t -> 'state -> ('state, 'error) handler_result;
  (** Called when connection is established to initialize 'state *)
  handle_data: string -> Connection.t -> 'state -> ('state, 'error) handler_result;
  (** Called when data arrives on the connection *)
  handle_error: 'error -> Connection.t -> 'state -> ('state, 'error) handler_result;
  (** Called when an 'error occurs *)
  handle_shutdown: Connection.t -> 'state -> ('state, 'error) handler_result;
  (** Called when server is shutting down *)
  handle_message: Message.t -> Connection.t -> 'state -> ('state, 'error) handler_result;
  (** Called when an inter-process message arrives *)
}

and t =
  | H: {
      handler: ('new_state, 'error) handler;
      state: 'new_state;
    } -> t

(**
   Default handler implementations.

   Include this in your handler module to get sensible defaults:

   ```ocaml module My_handler = struct include SocketPool.Handler.Default

   type state = my_state type error = my_error

   (* Override only what you need *) let handle_data data conn state = ... end
   ```
*)
val default: ('state, 'error) handler
