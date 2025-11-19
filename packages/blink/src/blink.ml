open Std
module Protocol = Protocol
module Transport = Transport
module Connection = Connection
module WebSocket = Websocket
module Error = Error

type error = Error.t
type message = Connection.message

let connect = Transport.connect
let request = Connection.request
let stream = Connection.stream
let messages = Connection.messages
let await = Connection.await
let close = Connection.close
(* test change *)
