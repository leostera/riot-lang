open Std


type ('ctx, 'err) state = {
  listener : Net.TcpListener.t;
  buffer_size : int;
  handler : (module Handler.Intf with type state = 'ctx and type error = 'err);
  initial_ctx : 'ctx;
  transport : Transport.t;
}

type internal_msg = Shutdown
type Message.t += AcceptorMsg of internal_msg

let rec loop state =
  let selector msg =
    match msg with AcceptorMsg msg -> `select msg | _ -> `skip
  in
  match receive ~selector () with
  | Shutdown -> ()
  | exception _ ->
      accept_connection state;
      loop state

and accept_connection state =
  match Net.TcpListener.accept state.listener with
  | Ok (stream, peer) ->
      let accepted_at = Time.Instant.now () in
      let conn_state =
        Connector.
          {
            transport = state.transport;
            stream;
            buffer_size = state.buffer_size;
            handler = state.handler;
            peer;
            accepted_at;
            ctx = state.initial_ctx;
          }
      in
      let _pid = Connector.start_link conn_state in
      ()
  | Error _ -> ()

let init state = loop state

let start_link state =
  let _pid =
    spawn (fun () ->
        init state;
        Ok ())
  in
  ()
