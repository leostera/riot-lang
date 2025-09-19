(** HTTP response representation **)

type t

val create : Status.t -> t
(** Create a new HTTP response **)

val status : t -> Status.t
(** Get the HTTP status code **)

val version : t -> Version.t
(** Get the HTTP version **)

val headers : t -> Header.t
(** Get the response headers **)

val body : t -> string option
(** Get the response body **)

val with_status : t -> Status.t -> t
(** Set the HTTP status **)

val with_version : t -> Version.t -> t
(** Set the HTTP version **)

val with_headers : t -> Header.t -> t
(** Set the response headers **)

val with_header : t -> Header.name -> Header.value -> t
(** Add/set a single header **)

val with_body : t -> string -> t
(** Set the response body **)

val without_body : t -> t
(** Remove the response body **)

val add_header : t -> Header.name -> Header.value -> t
(** Add a header (allows duplicates) **)

val remove_header : t -> Header.name -> t
(** Remove a header **)

val get_header : t -> Header.name -> Header.value option
(** Get a header value **)

val has_header : t -> Header.name -> bool
(** Check if header exists **)

(** Builder pattern for constructing responses **)
module Builder : sig
  type response = t
  type t

  val create : Status.t -> t
  val status : t -> Status.t -> t
  val version : t -> Version.t -> t
  val header : t -> Header.name -> Header.value -> t
  val headers : t -> Header.t -> t
  val body : t -> string -> t
  val build : t -> response
end

(** Convenience functions for common responses **)
val ok : string -> t
(** Create a 200 OK response with body **)

val created : string -> t
(** Create a 201 Created response with body **)

val accepted : string -> t
(** Create a 202 Accepted response with body **)

val no_content : unit -> t
(** Create a 204 No Content response **)

val bad_request : string -> t
(** Create a 400 Bad Request response with body **)

val unauthorized : string -> t
(** Create a 401 Unauthorized response with body **)

val forbidden : string -> t
(** Create a 403 Forbidden response with body **)

val not_found : string -> t
(** Create a 404 Not Found response with body **)

val method_not_allowed : string -> t
(** Create a 405 Method Not Allowed response with body **)

val internal_server_error : string -> t
(** Create a 500 Internal Server Error response with body **)

val not_implemented : string -> t
(** Create a 501 Not Implemented response with body **)

val bad_gateway : string -> t
(** Create a 502 Bad Gateway response with body **)

val service_unavailable : string -> t
(** Create a 503 Service Unavailable response with body **)
