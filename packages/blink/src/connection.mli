open Std

type t
type message =
  | Data of string
  | Done
  | Headers of Net.Http.Header.t
  | Status of Net.Http.Status.t
val make : reader:('socket, 'err) IO.Reader.t ->
writer:('socket, 'err) IO.Writer.t ->
of_io_error:('err -> Error.t) ->
uri:Net.Uri.t ->
t

val request : t -> Net.Http.Request.t -> ?body:string -> unit -> (unit, Error.t) result

val stream : t -> (message list, Error.t) result

val messages : ?on_message:(message list -> unit) -> t -> (message list, Error.t) result

val await : ?on_message:(message list -> unit) -> t -> (Net.Http.Response.t * string, Error.t) result

val close : t -> unit
