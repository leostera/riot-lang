(** # Middleware Framework

    Composable middleware for HTTP request/response handling.

    ## Example

    ```ocaml open Suri.Middleware

    let app =
    Pipeline.[ Logger.middleware; Router.middleware Router.[ get "/" (fun conn
              -> conn |> Conn.with_status Ok |> Conn.with_body "Hello!" |>
              Conn.send ); ]; ]

    let handler socket_conn req = let conn = Conn.make socket_conn req in let
    conn = Pipeline.run conn app in Conn.to_response conn ``` *)

module Conn = Conn
(** Connection context that flows through middleware *)

module Pipeline = Pipeline
(** Middleware pipeline execution *)

module Router = Router
(** HTTP routing with pattern matching *)
