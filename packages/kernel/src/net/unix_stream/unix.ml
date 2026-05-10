open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type t = int

type shutdown =
  | Read
  | Write
  | ReadWrite

type error =
  | InvalidSlice of { pos: int; len: int; buffer_len: int }
  | InvalidConnectState of { state: int }
  | WouldBlock
  | ConnectionRefused
  | ConnectionReset
  | TimedOut
  | BrokenPipe
  | NotConnected
  | ConnectionAborted
  | NetworkUnreachable
  | System of System_error.t

type connect_result =
  | Connected of t
  | InProgress of t

let connect_result_connected = 0

let connect_result_in_progress = 1

let shutdown_read = 0

let shutdown_write = 1

let shutdown_read_write = 2

module FFI = struct
  external connect: string -> (int * int, int) Result.t = "kernel_new_net_unix_stream_connect"

  external close: int -> (unit, int) Result.t = "kernel_new_net_socket_close"

  external finish_connect: int -> (unit, int) Result.t = "kernel_new_net_tcp_stream_finish_connect"

  external shutdown: int -> int -> (unit, int) Result.t = "kernel_new_net_tcp_stream_shutdown"

  external read: int -> bytes -> int -> int -> (int, int) Result.t =
    "kernel_new_net_tcp_stream_read"

  external write: int -> bytes -> int -> int -> (int, int) Result.t =
    "kernel_new_net_tcp_stream_write"

  external readv: int -> IO.IoVec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_readv"

  external writev: int -> IO.IoVec.t -> (int, int) Result.t = "kernel_new_net_tcp_stream_writev"
end

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error (InvalidSlice { pos; len; buffer_len = Bytes.length buf })
  else
    Result.Ok ()

let error_to_string = fun value ->
  match value with
  | InvalidSlice { pos; len; buffer_len } ->
      String.concat
        ""
        [
          "invalid buffer slice: pos=";
          Int.to_string pos;
          ", len=";
          Int.to_string len;
          ", buffer_len=";
          Int.to_string buffer_len;
        ]
  | InvalidConnectState { state } ->
      String.concat
        ""
        [ "invalid unix stream connect state returned by backend: "; Int.to_string state ]
  | WouldBlock -> "operation would block"
  | ConnectionRefused -> "connection refused"
  | ConnectionReset -> "connection reset by peer"
  | TimedOut -> "timed out"
  | BrokenPipe -> "broken pipe"
  | NotConnected -> "socket is not connected"
  | ConnectionAborted -> "connection aborted"
  | NetworkUnreachable -> "network unreachable"
  | System error -> System_error.to_string error

let error_of_system = fun value ->
  match value with
  | System_error.WouldBlock -> WouldBlock
  | System_error.ConnectionRefused -> ConnectionRefused
  | System_error.ConnectionReset -> ConnectionReset
  | System_error.TimedOut -> TimedOut
  | System_error.BrokenPipe -> BrokenPipe
  | System_error.NotConnected -> NotConnected
  | System_error.ConnectionAborted -> ConnectionAborted
  | System_error.NetworkUnreachable -> NetworkUnreachable
  | error -> System error

let shutdown_code = fun value ->
  match value with
  | Read -> shutdown_read
  | Write -> shutdown_write
  | ReadWrite -> shutdown_read_write

type shutdown_state = {
  fd: int;
  mutable read_shutdown: bool;
  mutable write_shutdown: bool;
}

type 'state cell = { mutable value: 'state }

let shutdown_states = { value = [] }

let rec find_shutdown_state = fun fd states ->
  match states with
  | [] -> None
  | state :: rest ->
      if state.fd = fd then
        Some state
      else
        find_shutdown_state fd rest

let ensure_shutdown_state = fun fd ->
  match find_shutdown_state fd shutdown_states.value with
  | Some state -> state
  | None ->
      let state = { fd; read_shutdown = false; write_shutdown = false } in
      shutdown_states.value <- state :: shutdown_states.value;
      state

let rec remove_shutdown_state = fun fd states ->
  match states with
  | [] -> []
  | state :: rest ->
      if state.fd = fd then
        rest
      else
        state :: remove_shutdown_state fd rest

let connect = fun path ->
  let* (fd, state) =
    FFI.connect path
    |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))
  in
  if state = connect_result_connected then
    Result.Ok (Connected fd)
  else if state = connect_result_in_progress then
    Result.Ok (InProgress fd)
  else
    Result.Error (InvalidConnectState { state })

let close = fun stream ->
  match FFI.close stream with
  | Result.Ok () ->
      shutdown_states.value <- remove_shutdown_state stream shutdown_states.value;
      Result.Ok ()
  | Result.Error code -> (
      let error = error_of_system (System_error.from_code code) in
      match error with
      | System System_error.BadFileDescriptor ->
          shutdown_states.value <- remove_shutdown_state stream shutdown_states.value;
          Result.Error error
      | _ -> Result.Error error
    )

let finish_connect = fun stream ->
  FFI.finish_connect stream
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let rec shutdown = fun stream how ->
  let state = ensure_shutdown_state stream in
  match how with
  | Write when state.write_shutdown -> Result.Ok ()
  | Read when state.read_shutdown -> Result.Ok ()
  | ReadWrite when state.read_shutdown && state.write_shutdown -> Result.Ok ()
  | ReadWrite when state.read_shutdown -> shutdown stream Write
  | ReadWrite when state.write_shutdown -> shutdown stream Read
  | _ ->
      let* () =
        FFI.shutdown stream (shutdown_code how)
        |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))
      in
      (
        match how with
        | Read -> state.read_shutdown <- true
        | Write -> state.write_shutdown <- true
        | ReadWrite ->
            state.read_shutdown <- true;
            state.write_shutdown <- true
      );
      Result.Ok ()

let read = fun stream ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  FFI.read stream buf pos len
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let write = fun stream ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(Bytes.length buf - pos) in
  let* () = validate_slice buf ~pos ~len in
  FFI.write stream buf pos len
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let read_vectored = fun stream iov ->
  FFI.readv stream iov
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let write_vectored = fun stream iov ->
  FFI.writev stream iov
  |> Result.map_err ~fn:(fun code -> error_of_system (System_error.from_code code))

let to_source = fun stream ->
  let module Source = struct
    type nonrec t = t

    let register = fun stream selector token interest ->
      Async.Adapter.Selector.register
        selector
        ~fd:stream
        ~token
        ~interest

    let reregister = fun stream selector token interest ->
      Async.Adapter.Selector.reregister
        selector
        ~fd:stream
        ~token
        ~interest

    let deregister = fun stream selector -> Async.Adapter.Selector.deregister selector ~fd:stream
  end in
  Async.Source.make (module Source) stream
