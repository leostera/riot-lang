(** HTTP request representation **)

type t

val create : Method.t -> Uri.t -> t
(** Create a new HTTP request **)

val method_ : t -> Method.t
(** Get the HTTP method **)

val uri : t -> Uri.t
(** Get the request URI **)

val version : t -> Version.t
(** Get the HTTP version **)

val headers : t -> Header.t
(** Get the request headers **)

val body : t -> string option
(** Get the request body **)

val with_method : t -> Method.t -> t
(** Set the HTTP method **)

val with_uri : t -> Uri.t -> t
(** Set the request URI **)

val with_version : t -> Version.t -> t
(** Set the HTTP version **)

val with_headers : t -> Header.t -> t
(** Set the request headers **)

val with_header : t -> Header.name -> Header.value -> t
(** Add/set a single header **)

val with_body : t -> string -> t
(** Set the request body **)

val without_body : t -> t
(** Remove the request body **)

val add_header : t -> Header.name -> Header.value -> t
(** Add a header (allows duplicates) **)

val remove_header : t -> Header.name -> t
(** Remove a header **)

val get_header : t -> Header.name -> Header.value option
(** Get a header value **)

val has_header : t -> Header.name -> bool
(** Check if header exists **)

(** Builder pattern for constructing requests **)
module Builder : sig
  type request = t
  type t

  val create : Method.t -> Uri.t -> t
  val method_ : t -> Method.t -> t
  val uri : t -> Uri.t -> t
  val version : t -> Version.t -> t
  val header : t -> Header.name -> Header.value -> t
  val headers : t -> Header.t -> t
  val body : t -> string -> t
  val build : t -> request
end

(** Convenience functions for common HTTP methods **)
val get : Uri.t -> t
(** Create a GET request **)

val post : Uri.t -> string -> t
(** Create a POST request with body **)

val put : Uri.t -> string -> t
(** Create a PUT request with body **)

val delete : Uri.t -> t
(** Create a DELETE request **)

val head : Uri.t -> t
(** Create a HEAD request **)

val options : Uri.t -> t
(** Create an OPTIONS request **)

val patch : Uri.t -> string -> t
(** Create a PATCH request with body **)
