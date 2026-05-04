(** HTTP request plus body data and remaining body metadata. *)
open Std

(** HTTP request with parsed headers and body. *)
type t

(** Create a request from parsed HTTP data. *)
val from_http: body:string -> Net.Http.Request.t -> t

(** Returns the HTTP method *)
val method_: t -> Net.Http.Method.t

(** Returns the request URI *)
val uri: t -> string

(** Returns the HTTP version *)
val version: t -> Net.Http.Version.t

(** Returns the request headers *)
val headers: t -> Net.Http.Header.t

(** Returns a request with the header set, replacing existing values. *)
val with_header: string -> string -> t -> t

(** Returns the request body (may be partial) *)
val body: t -> string

(** Returns bytes remaining to read for complete body *)
val remaining: t -> int
