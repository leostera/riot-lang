open Std

(** HTTP connection handle. *)
type t
(** Incremental message emitted while reading a response. *)
type message =
  | Data of string
  | Done
  | Headers of Net.Http.Header.t
  | Status of Net.Http.Status.t

(** Build a connection from reader and writer handles. *)
val make :
  ?on_close:(unit -> unit) ->
  reader:IO.Reader.t ->
  writer:IO.Writer.t ->
  uri:Net.Uri.t ->
  unit ->
  t

(** Send an HTTP request on the connection. *)
val request : t -> Net.Http.Request.t -> ?body:string -> unit -> (unit, Error.t) result

(** Read the next batch of response messages. *)
val stream : t -> (message list, Error.t) result

(** Collect messages until the response body completes. *)
val messages : ?on_message:(message list -> unit) -> t -> (message list, Error.t) result

(** Wait for the response to complete and return the response plus body. *)
val await :
  ?on_message:(message list -> unit) -> t -> (Net.Http.Response.t * string, Error.t) result

(** Close the connection. *)
val close : t -> unit
