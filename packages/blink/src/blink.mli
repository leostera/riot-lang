open Std
module Protocol : module type of Protocol
module Transport : module type of Transport
module Connection : module type of Connection
module WebSocket : module type of Websocket
module Error : module type of Error
module SSE : module type of Sse
module GRPC : module type of Grpc

type error = Error.t
type message = Connection.message

val connect : Net.Uri.t -> (Connection.t, error) result

val request :
  Connection.t ->
  Net.Http.Request.t ->
  ?body:string ->
  unit ->
  (unit, error) result

val stream : Connection.t -> (message list, error) result

val messages :
  ?on_message:(message list -> unit) ->
  Connection.t ->
  (message list, error) result

val await :
  ?on_message:(message list -> unit) ->
  Connection.t ->
  (Net.Http.Response.t * string, error) result

val close : Connection.t -> unit
