(** # Net.Http.Response - HTTP response representation
    
    HTTP response type with builder pattern for constructing responses.
    Immutable by default with functional update methods.
    
    ## Examples
    
    Creating responses:
    
    ```ocaml
    open Std.Net.Http
    
    (* Simple OK response *)
    let resp = Response.ok {|{"status":"success"}|}
      |> Response.with_header "Content-Type" "application/json" in
    
    (* Error response *)
    let resp = Response.not_found "Resource not found" in
    
    (* No content response *)
    let resp = Response.no_content () in
    ```
    
    Using the builder pattern:
    
    ```ocaml
    let resp = Response.Builder.create Status.Ok
      |> Response.Builder.header "Content-Type" "application/json"
      |> Response.Builder.header "Cache-Control" "no-cache"
      |> Response.Builder.body {|{"data":"value"}|}
      |> Response.Builder.build in
    ```
    
    Modifying responses:
    
    ```ocaml
    let resp = Response.create Status.Ok in
    let resp = resp
      |> Response.with_body {|{"message":"Hello"}|}
      |> Response.with_header "Content-Type" "application/json"
      |> Response.with_header "X-Request-ID" "abc123" in
    ```
    
    Inspecting responses:
    
    ```ocaml
    if Status.is_success (Response.status resp) then
      match Response.body resp with
      | Some body -> process_success body
      | None -> ()
    else
      Log.error "Request failed: %d" (Status.to_int (Response.status resp))
    ```
*)

type t
(** An HTTP response with status, headers, and optional body. *)

(** ## Construction *)

val create : Status.t -> t
(** Creates a new HTTP response with the given status code.
    
    ## Examples
    
    ```ocaml
    let resp = Response.create Status.Ok
    ```
*)

(** ## Access *)

val status : t -> Status.t
(** Returns the HTTP status code.
    
    ## Examples
    
    ```ocaml
    let status = Response.status resp in
    if Status.is_success status then
      Log.info "Success!"
    ```
*)

val version : t -> Version.t
(** Returns the HTTP version (defaults to HTTP/1.1).
    
    ## Examples
    
    ```ocaml
    Response.version resp  (* Version.Http11 *)
    ```
*)

val headers : t -> Header.t
(** Returns all response headers.
    
    ## Examples
    
    ```ocaml
    let headers = Response.headers resp in
    Header.iter (fun name value ->
      Printf.printf "%s: %s\n" name value
    ) headers
    ```
*)

val body : t -> string option
(** Returns the response body if present.
    
    ## Examples
    
    ```ocaml
    match Response.body resp with
    | Some body -> Log.info "Body: %s" body
    | None -> Log.info "No body"
    ```
*)

(** ## Modification *)

val with_status : t -> Status.t -> t
(** Returns a new response with the given status.
    
    ## Examples
    
    ```ocaml
    Response.with_status resp Status.Created
    ```
*)

val with_version : t -> Version.t -> t
(** Returns a new response with the given HTTP version.
    
    ## Examples
    
    ```ocaml
    Response.with_version resp Version.Http2
    ```
*)

val with_headers : t -> Header.t -> t
(** Returns a new response with the given headers (replaces all).
    
    ## Examples
    
    ```ocaml
    let headers = Header.empty
      |> Header.set "Content-Type" "text/html"
      |> Header.set "Cache-Control" "max-age=3600" in
    Response.with_headers resp headers
    ```
*)

val with_header : t -> Header.name -> Header.value -> t
(** Returns a new response with the header set (replaces existing).
    
    ## Examples
    
    ```ocaml
    Response.with_header resp "Content-Type" "application/json"
    ```
*)

val with_body : t -> string -> t
(** Returns a new response with the given body.
    
    ## Examples
    
    ```ocaml
    Response.with_body resp {|{"status":"ok"}|}
    ```
*)

val without_body : t -> t
(** Returns a new response without a body.
    
    ## Examples
    
    ```ocaml
    Response.without_body resp
    ```
*)

val add_header : t -> Header.name -> Header.value -> t
(** Returns a new response with the header added (allows duplicates).
    
    ## Examples
    
    ```ocaml
    resp
    |> Response.add_header "Set-Cookie" "session=abc"
    |> Response.add_header "Set-Cookie" "token=xyz"
    ```
*)

val remove_header : t -> Header.name -> t
(** Returns a new response with the header removed.
    
    ## Examples
    
    ```ocaml
    Response.remove_header resp "X-Debug-Info"
    ```
*)

val get_header : t -> Header.name -> Header.value option
(** Returns the first value for the given header name.
    
    ## Examples
    
    ```ocaml
    match Response.get_header resp "Content-Type" with
    | Some ct -> Printf.printf "Type: %s\n" ct
    | None -> ()
    ```
*)

val has_header : t -> Header.name -> bool
(** Checks if the response has the given header.
    
    ## Examples
    
    ```ocaml
    if Response.has_header resp "ETag" then
      Log.info "Response is cacheable"
    ```
*)

(** ## Builder Pattern *)

module Builder : sig
  (** Fluent builder for constructing HTTP responses.
      
      ## Examples
      
      ```ocaml
      let resp = Response.Builder.create Status.Created
        |> Response.Builder.header "Location" "/users/123"
        |> Response.Builder.header "Content-Type" "application/json"
        |> Response.Builder.body {|{"id":123,"name":"Alice"}|}
        |> Response.Builder.build
      ```
  *)

  type response = t
  (** The final response type *)

  type t
  (** The builder type *)

  val create : Status.t -> t
  (** Creates a new response builder. *)

  val status : t -> Status.t -> t
  (** Sets the status code. *)

  val version : t -> Version.t -> t
  (** Sets the HTTP version. *)

  val header : t -> Header.name -> Header.value -> t
  (** Adds a header. *)

  val headers : t -> Header.t -> t
  (** Sets all headers. *)

  val body : t -> string -> t
  (** Sets the response body. *)

  val build : t -> response
  (** Builds the final response. *)
end

(** ## Convenience Constructors *)

val ok : string -> t
(** Creates a 200 OK response with body.
    
    ## Examples
    
    ```ocaml
    let resp = Response.ok {|{"status":"success"}|}
      |> Response.with_header "Content-Type" "application/json"
    ```
*)

val created : string -> t
(** Creates a 201 Created response with body.
    
    ## Examples
    
    ```ocaml
    let resp = Response.created {|{"id":123}|}
      |> Response.with_header "Location" "/users/123"
    ```
*)

val accepted : string -> t
(** Creates a 202 Accepted response with body (async processing).
    
    ## Examples
    
    ```ocaml
    let resp = Response.accepted {|{"job_id":"abc123"}|}
    ```
*)

val no_content : unit -> t
(** Creates a 204 No Content response (successful, no body).
    
    ## Examples
    
    ```ocaml
    let resp = Response.no_content ()
    ```
*)

val bad_request : string -> t
(** Creates a 400 Bad Request response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.bad_request "Invalid JSON in request body"
    ```
*)

val unauthorized : string -> t
(** Creates a 401 Unauthorized response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.unauthorized "Authentication required"
      |> Response.with_header "WWW-Authenticate" "Bearer"
    ```
*)

val forbidden : string -> t
(** Creates a 403 Forbidden response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.forbidden "Access denied"
    ```
*)

val not_found : string -> t
(** Creates a 404 Not Found response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.not_found "User not found"
    ```
*)

val method_not_allowed : string -> t
(** Creates a 405 Method Not Allowed response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.method_not_allowed "POST not allowed on this resource"
      |> Response.with_header "Allow" "GET, HEAD"
    ```
*)

val internal_server_error : string -> t
(** Creates a 500 Internal Server Error response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.internal_server_error "Database connection failed"
    ```
*)

val not_implemented : string -> t
(** Creates a 501 Not Implemented response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.not_implemented "Feature not yet available"
    ```
*)

val bad_gateway : string -> t
(** Creates a 502 Bad Gateway response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.bad_gateway "Upstream service unavailable"
    ```
*)

val service_unavailable : string -> t
(** Creates a 503 Service Unavailable response with error message.
    
    ## Examples
    
    ```ocaml
    let resp = Response.service_unavailable "Maintenance in progress"
      |> Response.with_header "Retry-After" "3600"
    ```
*)
