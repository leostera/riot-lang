open Std

type topic = string

module Handler = struct
  type upgrade_opts = { do_upgrade: bool }

  type initialization_error = ..

  type error =
    | InitializationFailed of initialization_error
    | UnknownOpcode of int

  type ('state, 'error) result =
    | Continue of 'state
    | Push of Http.Ws.Frame.t list * 'state
    | Error of 'error

  type 'state handle_result = ('state, error) result

  type reported_error =
    | ReportedError: {
        error: error;
        render: error -> string;
      } -> reported_error

  let reported_error = fun (ReportedError { error; render = _ }) -> error

  let reported_error_to_string = fun (ReportedError { error; render }) -> render error

  module type Intf = sig
    type state
    type args
    val init: args -> state handle_result

    val handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> state -> state handle_result

    val handle_message: Message.t -> state -> state handle_result

    val error_to_string: error -> string
  end

  type t =
    | Pending: {
        pending_init: 'args -> 'state handle_result;
        pending_handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> 'state -> 'state handle_result;
        pending_handle_message: Message.t -> 'state -> 'state handle_result;
        pending_error_to_string: error -> string;
        pending_args: 'args;
      } -> t
    | Ready: {
        ready_handle_frame: Http.Ws.Frame.t -> Net.TcpStream.t -> 'state -> 'state handle_result;
        ready_handle_message: Message.t -> 'state -> 'state handle_result;
        ready_error_to_string: error -> string;
        ready_state: 'state;
      } -> t

  type operation_result = (t, reported_error) result

  let make (type a b) ((module I : Intf with type args = a and type state = b)) (args: a): t = Pending {
    pending_init = I.init;
    pending_handle_frame = I.handle_frame;
    pending_handle_message = I.handle_message;
    pending_error_to_string = I.error_to_string;
    pending_args = args;
  }

  let initialize = function
    | Ready _ as handler ->
        Continue handler
    | Pending {
      pending_init;
      pending_handle_frame;
      pending_handle_message;
      pending_error_to_string;
      pending_args
    } -> (
        match pending_init pending_args with
        | Continue state ->
            Continue (
              Ready {
                ready_handle_frame = pending_handle_frame;
                ready_handle_message = pending_handle_message;
                ready_error_to_string = pending_error_to_string;
                ready_state = state;
              }
            )
        | Push (frames, state) ->
            Push (
              frames,
              Ready {
                ready_handle_frame = pending_handle_frame;
                ready_handle_message = pending_handle_message;
                ready_error_to_string = pending_error_to_string;
                ready_state = state;
              }
            )
        | Error err -> Error (ReportedError { error = err; render = pending_error_to_string })
      )

  let init = fun handler _conn -> initialize handler

  let append_frames = fun prefix result ->
    match result with
    | Continue handler -> Push (prefix, handler)
    | Push (frames, handler) -> Push (prefix @ frames, handler)
    | Error err -> Error err

  let rec handle_frame = fun handler frame conn ->
    match handler with
    | Pending _ -> (
        match initialize handler with
        | Continue handler -> handle_frame handler frame conn
        | Push (frames, handler) -> append_frames frames (handle_frame handler frame conn)
        | Error err -> Error err
      )
    | Ready {
      ready_handle_frame;
      ready_handle_message;
      ready_error_to_string;
      ready_state
    } -> (
        match ready_handle_frame frame conn ready_state with
        | Continue state ->
            Continue (
              Ready {
                ready_handle_frame;
                ready_handle_message;
                ready_error_to_string;
                ready_state = state;
              }
            )
        | Push (frames, state) ->
            Push (
              frames,
              Ready {
                ready_handle_frame;
                ready_handle_message;
                ready_error_to_string;
                ready_state = state;
              }
            )
        | Error err -> Error (ReportedError { error = err; render = ready_error_to_string })
      )

  let rec handle_message = fun handler msg conn ->
    match handler with
    | Pending _ -> (
        match initialize handler with
        | Continue handler -> handle_message handler msg conn
        | Push (frames, handler) -> append_frames frames (handle_message handler msg conn)
        | Error err -> Error err
      )
    | Ready {
      ready_handle_frame;
      ready_handle_message;
      ready_error_to_string;
      ready_state
    } -> (
        match ready_handle_message msg ready_state with
        | Continue state ->
            Continue (
              Ready {
                ready_handle_frame;
                ready_handle_message;
                ready_error_to_string;
                ready_state = state;
              }
            )
        | Push (frames, state) ->
            Push (
              frames,
              Ready {
                ready_handle_frame;
                ready_handle_message;
                ready_error_to_string;
                ready_state = state;
              }
            )
        | Error err -> Error (ReportedError { error = err; render = ready_error_to_string })
      )

  module Default = struct
    let handle_frame = fun (frame: Http.Ws.Frame.t) _conn state ->
      match frame.opcode with
      | Ping -> Push ([ Http.Ws.Frame.pong () ], state)
      | _ -> Continue state

    let handle_message = fun _msg state -> Continue state

    let error_to_string = function
      | InitializationFailed _ -> "WebSocket handler initialization failed"
      | UnknownOpcode code -> "Unknown WebSocket opcode: " ^ Int.to_string code
  end

  module For_testing = struct
    let initialize = initialize

    let reported_error = reported_error

    let reported_error_to_string = reported_error_to_string
  end
end
