open Std

type ('s, 'e) conn_state = {
  transport: Transport.t;
  stream: Net.TcpStream.t;
  buffer_size: int;
  handler: ('s, 'e) Handler.handler;
  peer: Net.Addr.stream_addr;
  accepted_at: Time.Instant.t;
  ctx: 's;
}

type internal_msg =
  | Shutdown

exception Handshake_failed of Connection.error

type Message.t +=
  | ConnectorMsg of internal_msg

let mailbox_poll_timeout = Time.Duration.zero

let read_timeout = Time.Duration.from_secs 1

let rec loop: type s e. Connection.t -> (s, e) Handler.handler -> s -> unit = fun
  conn handler ctx ->
  (* Check for messages before blocking on TCP *)
  match receive_any ~timeout:mailbox_poll_timeout () with
  | msg ->
      (* Handle the actor message *)
      handle_message_internal msg conn handler ctx
  | exception Receive_timeout ->
      (* No messages, proceed to TCP I/O *)
      try_receive conn handler ctx

and handle_message_internal:
  type s e. Message.t ->
  Connection.t ->
  (s, e) Handler.handler ->
  s ->
  unit = fun msg conn handler ctx ->
  match handler.handle_message msg conn ctx with
  | Continue ctx -> loop conn handler ctx
  | Close ctx -> handler.handle_close conn ctx
  | Switch (Handler.H { handler = new_handler; state }) -> loop conn new_handler state
  | Error (_state, err) -> Log.error ("message handling error: " ^ (handler.to_string_error err))
  | Ok -> ()

and try_receive: type s e. Connection.t -> (s, e) Handler.handler -> s -> unit = fun
  conn handler ctx ->
  try
    match Connection.receive conn ~timeout:read_timeout with
    | Ok "" -> handler.handle_close conn ctx
    | Ok data -> handle_data data conn handler ctx
    | Error Connection.Closed -> handler.handle_close conn ctx
    | Error error ->
        Log.error ("connection receive error: " ^ Connection.error_to_string error);
        handler.handle_close conn ctx
  with
  | Syscall_timeout ->
      (* Timeout = no data available within 1ms, loop to check mailbox again *)
      loop conn handler ctx

and handle_data: type s e. string -> Connection.t -> (s, e) Handler.handler -> s -> unit = fun
  data conn handler ctx ->
  match handler.handle_data data conn ctx with
  | Continue ctx -> loop conn handler ctx
  | Close ctx -> handler.handle_close conn ctx
  | Switch (Handler.H { handler = new_handler; state }) -> handle_connection conn new_handler state
  | Error (_state, err) -> Log.error ("connection error: " ^ (handler.to_string_error err))
  | Ok -> ()

and handle_connection: type s e. Connection.t -> (s, e) Handler.handler -> s -> unit = fun
  conn handler ctx ->
  match handler.handle_connection conn ctx with
  | Continue ctx -> loop conn handler ctx
  | Close ctx -> handler.handle_close conn ctx
  | Switch (Handler.H { handler = new_handler; state }) -> handle_connection conn new_handler state
  | Error (_state, err) -> Log.error ("[Connector] Handler error: " ^ (handler.to_string_error err))
  | Ok -> ()

let init = fun state ->
  match Transport.handshake
    state.transport
    ~accepted_at:state.accepted_at
    ~stream:state.stream
    ~peer:state.peer
    ~buffer_size:state.buffer_size with
  | Ok conn ->
      handle_connection conn state.handler state.ctx;
      Connection.close conn;
      Ok ()
  | Error error ->
      Log.error "[Connector] Failed to handshake connection";
      Error (Handshake_failed error)

let spawn = fun state -> spawn (fun () -> init state)
