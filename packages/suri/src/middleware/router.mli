(** # HTTP Router

    Simple pattern-based HTTP router with parameter extraction.

    ## Example

    ```ocaml let routes =
    Router.[ get "/" handle_index; get "/users/:id" handle_user; post "/users"
            create_user; scope "/api" [ get "/health" handle_health; ]; ]

    let app = [ Router.middleware routes; ] ``` *)

type route
(** A single route definition *)

type t = route list
(** A router is a list of routes *)

val get : string -> Pipeline.middleware -> route
(** `GET` request handler *)

val post : string -> Pipeline.middleware -> route
(** `POST` request handler *)

val put : string -> Pipeline.middleware -> route
(** `PUT` request handler *)

val patch : string -> Pipeline.middleware -> route
(** `PATCH` request handler *)

val delete : string -> Pipeline.middleware -> route
(** `DELETE` request handler *)

val head : string -> Pipeline.middleware -> route
(** `HEAD` request handler *)

val scope : string -> route list -> route
(** Group routes under a path prefix *)

val middleware : t -> Pipeline.middleware
(** Convert routes to middleware that matches and dispatches *)
