(** # Net.Http.Request - HTTP request representation
    
    HTTP request type with builder pattern for constructing requests.
    Immutable by default with functional update methods.
    
    ## Examples
    
    Creating requests:
    
    ```ocaml
    open Std.Net.Http
    
    (* Simple GET request *)
    let uri = Uri.of_string "https://api.example.com/users" |> Result.unwrap in
    let req = Request.get uri in
    
    (* POST with body *)
    let uri = Uri.of_string "https://api.example.com/users" |> Result.unwrap in
    let body = {|{"name":"Alice","email":"alice@example.com"}|} in
    let req = Request.post uri body
      |> Request.with_header "Content-Type" "application/json" in
    ```
    
    Using the builder pattern:
    
    ```ocaml
    let uri = Uri.of_string "https://api.example.com/data" |> Result.unwrap in
    let req = Request.Builder.create Method.Post uri
      |> Request.Builder.header "Authorization" "Bearer token123"
      |> Request.Builder.header "Content-Type" "application/json"
      |> Request.Builder.body {|{"data":"value"}|}
      |> Request.Builder.build in
    ```
    
    Modifying requests:
    
    ```ocaml
    let req = Request.get uri in
    let req = req
      |> Request.with_header "Accept" "application/json"
      |> Request.with_header "User-Agent" "MyApp/1.0"
      |> Request.add_header "X-Custom" "value1"
      |> Request.add_header "X-Custom" "value2" in
    ```
    
    Inspecting requests:
    
    ```ocaml
    let method_ = Request.method_ req in
    let uri = Request.uri req in
    let headers = Request.headers req in
    
    match Request.body req with
    | Some body -> Log.info "Body: %s" body
    | None -> Log.info "No body"
    
    match Request.get_header req "Authorization" with
    | Some auth -> Log.info "Authenticated"
    | None -> Log.warn "No auth header"
    ```
*)

type t
(** An HTTP request with method, URI, headers, and optional body. *)

(** ## Construction *)

val create : Method.t -> Uri.t -> t
(** Creates a new HTTP request with the given method and URI.

    ## Examples

    ```ocaml let uri = Uri.of_string "https://example.com" |> Result.unwrap in
    let req = Request.create Method.Get uri ``` *)

(** ## Access *)

val method_ : t -> Method.t
(** Returns the HTTP method.

    ## Examples

    ```ocaml Request.method_ req (* Method.Get *) ``` *)

val uri : t -> Uri.t
(** Returns the request URI.

    ## Examples

    ```ocaml let uri = Request.uri req in Uri.to_string uri (*
    "https://example.com/path" *) ``` *)

val version : t -> Version.t
(** Returns the HTTP version (defaults to HTTP/1.1).

    ## Examples

    ```ocaml Request.version req (* Version.Http11 *) ``` *)

val headers : t -> Header.t
(** Returns all request headers.

    ## Examples

    ```ocaml let headers = Request.headers req in Header.iter (fun name value ->
    Printf.printf "%s: %s\n" name value ) headers ``` *)

val body : t -> string option
(** Returns the request body if present.

    ## Examples

    ```ocaml match Request.body req with | Some body -> process_body body | None
    -> () ``` *)

(** ## Modification *)

val with_method : t -> Method.t -> t
(** Returns a new request with the given method.

    ## Examples

    ```ocaml Request.with_method req Method.Post ``` *)

val with_uri : t -> Uri.t -> t
(** Returns a new request with the given URI.

    ## Examples

    ```ocaml let new_uri = Uri.of_string "https://api.v2.example.com" |>
    Result.unwrap in Request.with_uri req new_uri ``` *)

val with_version : t -> Version.t -> t
(** Returns a new request with the given HTTP version.

    ## Examples

    ```ocaml Request.with_version req Version.Http2 ``` *)

val with_headers : t -> Header.t -> t
(** Returns a new request with the given headers (replaces all).

    ## Examples

    ```ocaml let headers = Header.empty |> Header.set "Content-Type"
    "application/json" |> Header.set "Accept" "application/json" in
    Request.with_headers req headers ``` *)

val with_header : t -> Header.name -> Header.value -> t
(** Returns a new request with the header set (replaces existing).

    ## Examples

    ```ocaml Request.with_header req "Authorization" "Bearer token" ``` *)

val with_body : t -> string -> t
(** Returns a new request with the given body.
    
    ## Examples
    
    ```ocaml
    Request.with_body req {|{"name":"Alice"}|}
    ```
*)

val without_body : t -> t
(** Returns a new request without a body.

    ## Examples

    ```ocaml Request.without_body req ``` *)

val add_header : t -> Header.name -> Header.value -> t
(** Returns a new request with the header added (allows duplicates).

    ## Examples

    ```ocaml req |> Request.add_header "Accept" "text/html" |>
    Request.add_header "Accept" "application/json" ``` *)

val remove_header : t -> Header.name -> t
(** Returns a new request with the header removed.

    ## Examples

    ```ocaml Request.remove_header req "X-Debug" ``` *)

val get_header : t -> Header.name -> Header.value option
(** Returns the first value for the given header name.

    ## Examples

    ```ocaml match Request.get_header req "Content-Type" with | Some ct ->
    Printf.printf "Content-Type: %s\n" ct | None -> () ``` *)

val has_header : t -> Header.name -> bool
(** Checks if the request has the given header.

    ## Examples

    ```ocaml if Request.has_header req "Authorization" then Log.info "Request is
    authenticated" ``` *)

(** ## Builder Pattern *)

module Builder : sig
  (** Fluent builder for constructing HTTP requests.
      
      ## Examples
      
      ```ocaml
      let uri = Uri.of_string "https://api.example.com/data" |> Result.unwrap in
      let req = Request.Builder.create Method.Post uri
        |> Request.Builder.header "Authorization" "Bearer token"
        |> Request.Builder.header "Content-Type" "application/json"
        |> Request.Builder.body {|{"key":"value"}|}
        |> Request.Builder.build
      ```
  *)

  type request = t
  (** The final request type *)

  type t
  (** The builder type *)

  val create : Method.t -> Uri.t -> t
  (** Creates a new request builder. *)

  val method_ : t -> Method.t -> t
  (** Sets the HTTP method. *)

  val uri : t -> Uri.t -> t
  (** Sets the URI. *)

  val version : t -> Version.t -> t
  (** Sets the HTTP version. *)

  val header : t -> Header.name -> Header.value -> t
  (** Adds a header. *)

  val headers : t -> Header.t -> t
  (** Sets all headers. *)

  val body : t -> string -> t
  (** Sets the request body. *)

  val build : t -> request
  (** Builds the final request. *)
end

(** ## Convenience Constructors *)

val get : Uri.t -> t
(** Creates a GET request.

    ## Examples

    ```ocaml let uri = Uri.of_string "https://api.example.com/users" |>
    Result.unwrap in let req = Request.get uri ``` *)

val post : Uri.t -> string -> t
(** Creates a POST request with body.
    
    ## Examples
    
    ```ocaml
    let uri = Uri.of_string "https://api.example.com/users" |> Result.unwrap in
    let body = {|{"name":"Alice"}|} in
    let req = Request.post uri body
      |> Request.with_header "Content-Type" "application/json"
    ```
*)

val put : Uri.t -> string -> t
(** Creates a PUT request with body.
    
    ## Examples
    
    ```ocaml
    let req = Request.put uri {|{"name":"Bob"}|}
    ```
*)

val delete : Uri.t -> t
(** Creates a DELETE request.

    ## Examples

    ```ocaml let req = Request.delete uri ``` *)

val head : Uri.t -> t
(** Creates a HEAD request (like GET but no body).

    ## Examples

    ```ocaml let req = Request.head uri ``` *)

val options : Uri.t -> t
(** Creates an OPTIONS request (query allowed methods).

    ## Examples

    ```ocaml let req = Request.options uri ``` *)

val patch : Uri.t -> string -> t
(** Creates a PATCH request with body (partial update).
    
    ## Examples
    
    ```ocaml
    let req = Request.patch uri {|{"email":"new@example.com"}|}
    ```
*)
