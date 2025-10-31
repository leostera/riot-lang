(** Connection handler abstraction with protocol switching support.

    Handlers define how connections are processed through lifecycle hooks. The
    key feature is the ability to switch handlers mid-connection via the
    [Switch] result, enabling protocol upgrades (e.g., HTTP → WebSocket). *)



module rec R : sig
  type t =
    | H : {
        handler :
          (module R.Intf with type state = 'new_state and type error = 'error);
        state : 'new_state;
      }
        -> t
        (** Handler GADT that allows switching between different handler types
        *)

  type ('state, 'error) handler_result =
    | Ok  (** Handler completed successfully *)
    | Continue of 'state  (** Continue with updated state *)
    | Close of 'state  (** Close the connection gracefully *)
    | Error of 'state * 'error  (** Connection error occurred *)
    | Switch of t
        (** Switch to a different handler (e.g., WebSocket upgrade) *)

  (** Handler interface for processing connections.

      Handlers define how connections are processed through a series of hooks:
      - [handle_connection] - Initialize connection state
      - [handle_data] - Process incoming data
      - [handle_error] - Handle errors
      - [handle_close] - Clean up when closing
      - [handle_shutdown] - Handle graceful shutdown
      - [handle_timeout] - Handle timeout events
      - [handle_message] - Handle inter-process messages *)
  module type Intf = sig
    type state
    (** The state maintained throughout the connection lifecycle *)

    type error
    (** Error type for this handler *)

    val to_string_error : error -> string
    (** Convert error to string for logging *)

    val handle_close : Connection.t -> state -> unit
    (** Called when connection is closing for cleanup *)

    val handle_connection :
      Connection.t -> state -> (state, error) handler_result
    (** Called when connection is established to initialize state *)

    val handle_data :
      string -> Connection.t -> state -> (state, error) handler_result
    (** Called when data arrives on the connection *)

    val handle_error :
      error -> Connection.t -> state -> (state, error) handler_result
    (** Called when an error occurs *)

    val handle_shutdown : Connection.t -> state -> (state, error) handler_result
    (** Called when server is shutting down *)

    val handle_message :
      Message.t -> Connection.t -> state -> (state, error) handler_result
    (** Called when an inter-process message arrives *)
  end
end

type t = R.t =
  | H : {
      handler :
        (module R.Intf with type state = 'new_state and type error = 'error);
      state : 'new_state;
    }
      -> t

type ('state, 'error) handler_result = ('state, 'error) R.handler_result =
  | Ok
  | Continue of 'state
  | Close of 'state
  | Error of 'state * 'error
  | Switch of t

module type Intf = R.Intf

(** Default handler implementations.

    Include this in your handler module to get sensible defaults:

    ```ocaml module My_handler = struct include SocketPool.Handler.Default

    type state = my_state type error = my_error

    (* Override only what you need *) let handle_data data conn state = ... end
    ``` *)
module Default : sig
  val to_string_error : 'error -> string
  val handle_close : Connection.t -> 'state -> unit

  val handle_connection :
    Connection.t -> 'state -> ('state, 'error) handler_result

  val handle_data :
    string -> Connection.t -> 'state -> ('state, 'error) handler_result

  val handle_error :
    'error -> Connection.t -> 'state -> ('state, 'error) handler_result

  val handle_shutdown :
    Connection.t -> 'state -> ('state, 'error) handler_result

  val handle_message :
    Message.t -> Connection.t -> 'state -> ('state, 'error) handler_result
end

val to_string_error :
  (module Intf with type state = 's and type error = 'e) -> 'e -> string
(** Helper to call [to_string_error] on a handler module *)

val handle_close :
  (module Intf with type state = 's and type error = 'e) ->
  Connection.t ->
  's ->
  unit
(** Helper to call [handle_close] on a handler module *)

val handle_connection :
  (module Intf with type state = 's and type error = 'e) ->
  Connection.t ->
  's ->
  ('s, 'e) handler_result
(** Helper to call [handle_connection] on a handler module *)

val handle_data :
  (module Intf with type state = 's and type error = 'e) ->
  string ->
  Connection.t ->
  's ->
  ('s, 'e) handler_result
(** Helper to call [handle_data] on a handler module *)

val handle_message :
  (module Intf with type state = 's and type error = 'e) ->
  Message.t ->
  Connection.t ->
  's ->
  ('s, 'e) handler_result
(** Helper to call [handle_message] on a handler module *)
