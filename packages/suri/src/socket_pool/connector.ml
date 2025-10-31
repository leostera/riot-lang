open Std


type ('s, 'e) conn_state = {
  transport : Transport.t;
  stream : Net.TcpStream.t;
  buffer_size : int;
  handler : (module Handler.Intf with type error = 'e and type state = 's);
  peer : Net.Addr.stream_addr;
  accepted_at : Time.Instant.t;
  ctx : 's;
}

type internal_msg = Shutdown
type Message.t += ConnectorMsg of internal_msg

let rec loop : type s e.
    Connection.t ->
    (module Handler.Intf with type state = s and type error = e) ->
    s ->
    unit =
 fun conn handler ctx ->
  let module H = (val handler) in
  let selector msg =
    match msg with ConnectorMsg msg -> `select msg | _ -> `skip
  in
  match receive ~selector () with
  | Shutdown -> H.handle_close conn ctx
  | exception _ -> try_receive conn handler ctx

and try_receive : type s e.
    Connection.t ->
    (module Handler.Intf with type state = s and type error = e) ->
    s ->
    unit =
 fun conn handler ctx ->
  let module H = (val handler) in
  match Connection.receive conn with
  | Ok "" -> H.handle_close conn ctx
  | Ok data -> handle_data data conn handler ctx
  | Error `Closed -> H.handle_close conn ctx

and handle_data : type s e.
    string ->
    Connection.t ->
    (module Handler.Intf with type state = s and type error = e) ->
    s ->
    unit =
 fun data conn handler ctx ->
  let module H = (val handler) in
  match H.handle_data data conn ctx with
  | Continue ctx -> loop conn handler ctx
  | Close ctx -> H.handle_close conn ctx
  | Switch (Handler.H { handler; state }) ->
      handle_connection conn handler state
  | Error (_state, err) ->
      Log.error "connection error: %s" (H.to_string_error err)
  | Ok -> ()

and handle_connection : type s e.
    Connection.t ->
    (module Handler.Intf with type state = s and type error = e) ->
    s ->
    unit =
 fun conn handler ctx ->
  let module H = (val handler) in
  match H.handle_connection conn ctx with
  | Continue ctx -> loop conn handler ctx
  | Close ctx -> H.handle_close conn ctx
  | Switch (Handler.H { handler; state }) ->
      handle_connection conn handler state
  | Error (_state, err) ->
      Log.error "connection error: %s" (H.to_string_error err)
  | Ok -> ()

let init state =
  match
    Transport.handshake state.transport ~accepted_at:state.accepted_at
      ~stream:state.stream ~peer:state.peer ~buffer_size:state.buffer_size
  with
  | Ok conn ->
      handle_connection conn state.handler state.ctx;
      Connection.close conn
  | Error _ -> Log.error "failed to handshake connection"

let start_link state =
  let _pid =
    spawn (fun () ->
        init state;
        Ok ())
  in
  ()
