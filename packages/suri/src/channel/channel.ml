open Std

type topic = string

module Handler = struct
  type upgrade_opts = { do_upgrade: bool }

  type ('state, 'error) handle_result = [
    | `push of Http.Ws.Frame.t list * 'state
    | `ok of 'state
    | `error of 'state * 'error
  ]

  module type Intf = sig
    type state
    type args
    val init: args -> (state, [> `Unknown_opcode of int]) handle_result

    val handle_frame:
      Http.Ws.Frame.t ->
      Net.TcpStream.t ->
      state ->
      (state, [> `Unknown_opcode of int]) handle_result

    val handle_message: Message.t -> state -> (state, [> `Unknown_opcode of int]) handle_result
  end

  type t =
    | H: (module Intf with type args = 'a and type state = 'b) * 'b -> t

  let make (type a b) ((module I : Intf with type args = a and type state = b)) (args: a): t =
    match I.init args with
    | `ok state -> H ((module I), state)
    | `push (_, state) -> H ((module I), state)
    | `error (state, _) -> H ((module I), state)

  let init = fun (H ((module I), state)) _conn -> `continue (_conn, H ((module I), state))

  let handle_frame = fun (H ((module I), state)) frame conn ->
    match I.handle_frame frame conn state with
    | `ok state -> `continue (conn, H ((module I), state))
    | `push (frames, state) -> `push (frames, H ((module I), state))
    | `error (state, err) -> `error (conn, err)

  let handle_message = fun (H ((module I), state)) msg conn ->
    match I.handle_message msg state with
    | `ok state -> `continue (conn, H ((module I), state))
    | `push (frames, state) -> `push (frames, H ((module I), state))
    | `error (state, err) -> `error (conn, err)

  module Default = struct
    let handle_frame = fun (frame: Http.Ws.Frame.t) _conn state ->
      match frame.opcode with
      | Ping -> `push ([ Http.Ws.Frame.pong () ], state)
      | _ -> `ok state

    let handle_message = fun _msg state -> `ok state
  end
end
