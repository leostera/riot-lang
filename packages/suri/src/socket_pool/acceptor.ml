open Std

type ('ctx, 'err) state = {
  listener: Net.TcpListener.t;
  buffer_size: int;
  handler: ('ctx, 'err) Handler.handler;
  initial_ctx: 'ctx;
  transport: Transport.t;
}

type internal_msg =
  | Shutdown

type Message.t +=
  | AcceptorMsg of internal_msg

let receive_selector = fun msg ->
  match msg with
  | AcceptorMsg msg -> Select msg
  | _ -> Skip

let listener_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Net.TcpListener.Connection_refused -> "connection refused"
  | Net.TcpListener.Closed -> "closed"
  | Net.TcpListener.System_error error -> IO.error_message error

let rec loop = fun state ->
  match receive ~selector:receive_selector ~timeout:(Time.Duration.from_millis 5) () with
  | Shutdown -> ()
  | exception Receive_timeout ->
      accept_connection state;
      loop state

and accept_connection = fun state ->
  match Net.TcpListener.accept state.listener with
  | Ok (stream, peer) ->
      let accepted_at = Time.Instant.now () in
      let conn_state =
        Connector.{
          transport = state.transport;
          stream;
          buffer_size = state.buffer_size;
          handler = state.handler;
          peer;
          accepted_at;
          ctx = state.initial_ctx;
        }
      in
      let _pid = Connector.spawn conn_state in
      ()
  | Error error -> Log.error ("accept failed: " ^ listener_error_to_string error)

let init = fun state ->
  loop state;
  Ok ()

let spawn = fun state -> spawn (fun () -> init state)
