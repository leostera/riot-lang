(** # HTTP Request Representation

    Wraps the parsed HTTP request with body data and metadata. *)

open Std

type t
(** HTTP request with parsed headers and body *)

val of_http : body:string -> Net.Http.Request.t -> t
(** Create a request from parsed HTTP data *)

val method_ : t -> Net.Http.Method.t
(** Returns the HTTP method *)

val uri : t -> string
(** Returns the request URI *)

val version : t -> Net.Http.Version.t
(** Returns the HTTP version *)

val headers : t -> Net.Http.Header.t
(** Returns the request headers *)

val body : t -> string
(** Returns the request body (may be partial) *)

val remaining : t -> int
(** Returns bytes remaining to read for complete body *)
