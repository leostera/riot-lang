(** # HTTP/1.1 Protocol Handler

    Implements HTTP/1.1 request parsing, response sending, and connection
    management with keep-alive support.

    ## Features

    - HTTP/1.1 request parsing with configurable limits
    - Keep-alive connection management

    ## Example

    ```ocaml let handler _conn req = let body = Request.body req in Response.ok
    ~body:"Hello, World!" ()

    let config = Config.make () in let state = Http1.make_handler ~config
    ~handler () in (* Use with Socket_pool *) ``` *)

type state
type error =
[
  | `ParseError of string
  | `ExcessBodyRead
  | `IoError of string
]
val to_string_error: error -> string
(** Create a handler that supports WebSocket upgrades via {!Http_handler.response}. *)
val make_handler:
  config:Super.Config.t -> handler:Http_handler.t -> ?sniffed_data:string -> unit -> state
(** Handler functions for Socket_pool integration *)
val handle_close: Socket_pool.Connection.t -> state -> unit

val handle_connection: Socket_pool.Connection.t -> state -> (state, error) Socket_pool.Handler.handler_result

val handle_data: string -> Socket_pool.Connection.t -> state -> (state, error) Socket_pool.Handler.handler_result

val handle_error: error -> Socket_pool.Connection.t -> state -> (state, error) Socket_pool.Handler.handler_result

val handle_shutdown: Socket_pool.Connection.t -> state -> (state, error) Socket_pool.Handler.handler_result

val handle_message:
  Std.Message.t -> Socket_pool.Connection.t -> state -> (state, error) Socket_pool.Handler.handler_result
