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

open Std
open Miniriot

type handler
type state
type error = [ `ParseError of string | `ExcessBodyRead | `IoError of string ]

val to_string_error : error -> string

val make_handler :
  config:Config.t ->
  handler:(Socket_pool.Connection.t -> Request.t -> Response.t) ->
  ?sniffed_data:string ->
  unit ->
  state

include
  Socket_pool.Handler.Intf with type state := state and type error := error
