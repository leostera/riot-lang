(** # HTTP Request Representation

    Wraps the parsed HTTP request with body data and metadata. *)
open Std

(** HTTP request with parsed headers and body *)
(** Create a request from parsed HTTP data *)
type t
val of_http: body:string -> Net.Http.Request.t -> t

(** Returns the HTTP method *)
val method_: t -> Net.Http.Method.t

(** Returns the request URI *)
val uri: t -> string

(** Returns the HTTP version *)
val version: t -> Net.Http.Version.t

(** Returns the request headers *)
val headers: t -> Net.Http.Header.t

(** Returns the request body (may be partial) *)
val body: t -> string

(** Returns bytes remaining to read for complete body *)
val remaining: t -> int
