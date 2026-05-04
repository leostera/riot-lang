(** TCP stream for connected sockets *)
open Global
open IO
open Kernel.Async

type t = Kernel.Net.TcpStream.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let io_error_of_tcp_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Net.TcpStream.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Net.TcpStream.InvalidSocketAddr _ -> IO.Invalid_argument
  | Kernel.Net.TcpStream.InvalidConnectState _ -> IO.Invalid_argument
  | Kernel.Net.TcpStream.WouldBlock -> IO.Operation_would_block
  | Kernel.Net.TcpStream.ConnectionRefused -> IO.Connection_refused
  | Kernel.Net.TcpStream.ConnectionReset -> IO.Connection_reset_by_peer
  | Kernel.Net.TcpStream.TimedOut -> IO.Connection_timed_out
  | Kernel.Net.TcpStream.BrokenPipe -> IO.Broken_pipe
  | Kernel.Net.TcpStream.NotConnected -> IO.Transport_endpoint_not_connected
  | Kernel.Net.TcpStream.ConnectionAborted -> IO.Software_caused_connection_abort
  | Kernel.Net.TcpStream.NetworkUnreachable -> IO.Network_is_unreachable
  | Kernel.Net.TcpStream.System error -> IO.from_system_error error

let connect = fun addr ->
  let rec finish_connect stream =
    let source = Kernel.Net.TcpStream.to_source stream in
    match Kernel.Net.TcpStream.finish_connect stream with
    | Ok () -> Ok stream
    | Error Kernel.Net.TcpStream.WouldBlock ->
        Runtime.syscall
          ~name:"TcpStream.connect"
          ~interest:Interest.writable
          ~source
          (fun () -> finish_connect stream)
    | Error Kernel.Net.TcpStream.ConnectionRefused -> Error Connection_refused
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  let rec connect_loop () =
    match Kernel.Net.TcpStream.connect addr with
    | Ok (Kernel.Net.TcpStream.Connected stream) -> Ok stream
    | Ok (Kernel.Net.TcpStream.InProgress stream) -> finish_connect stream
    | Error Kernel.Net.TcpStream.ConnectionRefused -> Error Connection_refused
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  connect_loop ()

let read = fun stream buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some l -> l
  in
  let source = Kernel.Net.TcpStream.to_source stream in
  (* Transform Time.Duration.t to float seconds for Runtime.syscall *)
  let timeout = Option.map timeout ~fn:Time.Duration.to_secs_float in
  let rec read_loop () =
    match Kernel.Net.TcpStream.read stream buffer ~pos ~len with
    | Ok 0 -> Error Closed
    | Ok bytes_read -> Ok bytes_read
    | Error Kernel.Net.TcpStream.WouldBlock ->
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
  let source = Kernel.Net.TcpStream.to_source stream in
  let rec write_loop () =
    match Kernel.Net.TcpStream.write stream buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error Kernel.Net.TcpStream.WouldBlock ->
        Runtime.syscall
          ~name:"TcpStream.write"
          ~interest:Interest.writable
          ~source
          (fun () -> write_loop ())
    | Error err -> Error (System_error (io_error_of_tcp_error err))
  in
  write_loop ()

let close = fun stream ->
  match Kernel.Net.TcpStream.close stream with
  | Ok () -> ()
  | Error _ -> ()

let to_reader = fun stream ->
  let module Read = struct
    type nonrec t = t

    let read = fun t ~into ->
      let writable =
        if IO.Buffer.writable_bytes into = 0 then (
          match IO.Buffer.ensure_free into 4_096 with
          | Ok () -> IO.Buffer.writable into
          | Error error ->
              Kernel.SystemError.panic
                ("Net.TcpStream.to_reader.ensure_free: " ^ Kernel.IO.Error.message error)
        ) else
          IO.Buffer.writable into
      in
      let source = Kernel.Net.TcpStream.to_source t in
      let rec loop () =
        match Kernel.Net.TcpStream.read_vectored t (IO.IoVec.from_slices [|writable|]) with
        | Ok n -> (
            match IO.Buffer.commit into n with
            | Ok () -> Ok n
            | Error error ->
                Kernel.SystemError.panic
                  ("Net.TcpStream.to_reader.commit: " ^ Kernel.IO.Error.message error)
          )
        | Error Kernel.Net.TcpStream.WouldBlock ->
            Runtime.syscall ~name:"TcpStream.read" ~interest:Interest.readable ~source loop
        | Error err -> Error (io_error_of_tcp_error err)
      in
      loop ()

    let read_vectored = fun t ~into:bufs ->
      let source = Kernel.Net.TcpStream.to_source t in
      let rec loop () =
        match Kernel.Net.TcpStream.read_vectored t bufs with
        | Ok n -> Ok n
        | Error Kernel.Net.TcpStream.WouldBlock ->
            Runtime.syscall ~name:"TcpStream.read_vectored" ~interest:Interest.readable ~source loop
        | Error err -> Error (io_error_of_tcp_error err)
      in
      loop ()

    let is_read_vectored = fun _t -> true
  end in
  IO.Reader.from_source (module Read) stream

let to_writer = fun stream ->
  let module Write = struct
    type nonrec t = t

    let write = fun t ~from ->
      let source = Kernel.Net.TcpStream.to_source t in
      let rec loop () =
        match Kernel.Net.TcpStream.write_vectored t (IO.Buffer.to_iovec from) with
        | Ok n -> Ok n
        | Error Kernel.Net.TcpStream.WouldBlock ->
            Runtime.syscall ~name:"TcpStream.write" ~interest:Interest.writable ~source loop
        | Error err -> Error (io_error_of_tcp_error err)
      in
      loop ()

    let write_vectored = fun t ~from:bufs ->
      let source = Kernel.Net.TcpStream.to_source t in
      let rec loop () =
        match Kernel.Net.TcpStream.write_vectored t bufs with
        | Ok n -> Ok n
        | Error Kernel.Net.TcpStream.WouldBlock ->
            Runtime.syscall
              ~name:"TcpStream.write_vectored"
              ~interest:Interest.writable
              ~source
              loop
        | Error err -> Error (io_error_of_tcp_error err)
      in
      loop ()

    let flush = fun _t -> Ok ()
  end in
  IO.Writer.from_sink (module Write) stream
