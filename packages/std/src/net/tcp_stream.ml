(** TCP stream for connected sockets *)
open Global
open IO
open Kernel.Async

type t = Kernel.Net.Tcp_stream.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let io_error_of_tcp_error = function
  | Kernel.Net.Tcp_stream.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Net.Tcp_stream.InvalidSocketAddr _ -> IO.Invalid_argument
  | Kernel.Net.Tcp_stream.InvalidConnectState _ -> IO.Invalid_argument
  | Kernel.Net.Tcp_stream.WouldBlock -> IO.Operation_would_block
  | Kernel.Net.Tcp_stream.ConnectionRefused -> IO.Connection_refused
  | Kernel.Net.Tcp_stream.ConnectionReset -> IO.Connection_reset_by_peer
  | Kernel.Net.Tcp_stream.TimedOut -> IO.Connection_timed_out
  | Kernel.Net.Tcp_stream.BrokenPipe -> IO.Broken_pipe
  | Kernel.Net.Tcp_stream.NotConnected -> IO.Transport_endpoint_not_connected
  | Kernel.Net.Tcp_stream.ConnectionAborted -> IO.Software_caused_connection_abort
  | Kernel.Net.Tcp_stream.NetworkUnreachable -> IO.Network_is_unreachable
  | Kernel.Net.Tcp_stream.System error -> IO.of_system_error error

let connect = fun addr ->
  let rec finish_connect stream =
    let source = Kernel.Net.Tcp_stream.to_source stream in
    match Kernel.Net.Tcp_stream.finish_connect stream with
    | Ok () -> Ok stream
    | Error Kernel.Net.Tcp_stream.WouldBlock ->
        Runtime.syscall
          ~name:"TcpStream.connect"
          ~interest:Interest.writable
          ~source
          (fun () -> finish_connect stream)
    | Error Kernel.Net.Tcp_stream.ConnectionRefused ->
        Error Connection_refused
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  let rec connect_loop () =
    match Kernel.Net.Tcp_stream.connect addr with
    | Ok (Kernel.Net.Tcp_stream.Connected stream) ->
        Ok stream
    | Ok (Kernel.Net.Tcp_stream.InProgress stream) ->
        finish_connect stream
    | Error Kernel.Net.Tcp_stream.ConnectionRefused ->
        Error Connection_refused
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  connect_loop ()

let read = fun stream buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  (* Transform Time.Duration.t to float seconds for Runtime.syscall *)
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  let rec read_loop () =
    match Kernel.Net.Tcp_stream.read stream buffer ~pos ~len with
    | Ok 0 -> Error Closed
    | Ok bytes_read -> Ok bytes_read
    | Error Kernel.Net.Tcp_stream.WouldBlock ->
        Runtime.syscall
          ?timeout
          ~name:"TcpStream.read"
          ~interest:Interest.readable
          ~source
          (fun () -> read_loop ())
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  read_loop ()

let write = fun stream buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  let rec write_loop () =
    match Kernel.Net.Tcp_stream.write stream buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error Kernel.Net.Tcp_stream.WouldBlock ->
        Runtime.syscall
          ~name:"TcpStream.write"
          ~interest:Interest.writable
          ~source
          (fun () -> write_loop ())
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  write_loop ()

let close = fun stream ->
  ignore (Kernel.Net.Tcp_stream.close stream)

let to_reader = fun stream ->
  let module Read = struct
    type nonrec t = t

    type nonrec err = error

    let read = fun t ?timeout:_ buf ->
      (* Note: timeout parameter ignored for now - Actors handles suspension *)
      read t buf ()

    let read_vectored = fun t bufs ->
      let source = Kernel.Net.Tcp_stream.to_source t in
      let rec loop () =
        match Kernel.Net.Tcp_stream.read_vectored t bufs with
        | Ok n -> Ok n
        | Error Kernel.Net.Tcp_stream.WouldBlock ->
            Runtime.syscall
              ~name:"TcpStream.read_vectored"
              ~interest:Interest.readable
              ~source
              loop
        | Error err -> Error (System_error (io_error_of_tcp_error err))
      in
      loop ()

    let direct_string = fun _t -> None
  end in
  IO.Reader.of_read_src (module Read) stream

let to_writer = fun stream ->
  let module Write = struct
    type nonrec t = t

    type nonrec err = error

    let write = fun t ~buf ->
      let bytes = Bytes.of_string buf in
      match write t bytes () with
      | Ok n -> Ok n
      | Error e -> Error e

    let write_owned_vectored = fun t ~bufs ->
      let source = Kernel.Net.Tcp_stream.to_source t in
      let rec loop () =
        match Kernel.Net.Tcp_stream.write_vectored t bufs with
        | Ok n -> Ok n
        | Error Kernel.Net.Tcp_stream.WouldBlock ->
            Runtime.syscall
              ~name:"TcpStream.write_vectored"
              ~interest:Interest.writable
              ~source
              loop
        | Error err -> Error (System_error (io_error_of_tcp_error err))
      in
      loop ()

    let flush = fun _t -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) stream
