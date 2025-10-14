(** # Middleware Pipeline

    Execute a sequence of middleware functions on a connection.

    ## Example

    ```ocaml let app =
    [ Logger.middleware; Router.middleware routes; my_handler; ]

    let handler socket_conn req = let conn = Conn.make socket_conn req in let
    conn = Pipeline.run conn app in Conn.to_response conn ``` *)

type middleware = Conn.t -> Conn.t
(** A middleware function transforms a connection *)

type t = middleware list
(** A pipeline is a list of middleware *)

val run : Conn.t -> t -> Conn.t
(** Run a pipeline on a connection, stopping if halted *)
