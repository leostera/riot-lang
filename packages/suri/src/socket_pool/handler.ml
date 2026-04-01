open Std

type ('state, 'error) handler = {
  to_string_error: 'error -> string;
  handle_close: Connection.t -> 'state -> unit;
  handle_connection: Connection.t -> 'state -> ('state, 'error) handler_result;
  handle_data: string -> Connection.t -> 'state -> ('state, 'error) handler_result;
  handle_error: 'error -> Connection.t -> 'state -> ('state, 'error) handler_result;
  handle_shutdown: Connection.t -> 'state -> ('state, 'error) handler_result;
  handle_message: Message.t -> Connection.t -> 'state -> ('state, 'error) handler_result;
}

and t =
  | H: {
      handler: ('new_state, 'error) handler;
      state: 'new_state;
    } -> t

and ('state, 'error) handler_result =
  | Ok
  | Continue of 'state
  | Close of 'state
  | Error of 'state * 'error
  | Switch of t

let default = {
  to_string_error = (fun _err -> "unknown error");
  handle_close = (fun _sock _state -> ());
  handle_connection = (fun _sock state -> Continue state);
  handle_data = (fun _data _sock state -> Continue state);
  handle_error = (fun err _sock state -> Error (state, err));
  handle_shutdown = (fun _sock _state -> Ok);
  handle_message = (fun _msg _conn state -> Continue state);
}
