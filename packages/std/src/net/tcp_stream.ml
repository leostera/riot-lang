(** TCP stream for connected sockets *)
open Global
open IO
open Kernel.Async

type t = Kernel.Net.Tcp_stream.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let connect = fun addr ->
  let rec connect_loop = fun () ->
    match Kernel.Net.Tcp_stream.connect addr with
    | Ok (`Connected stream) ->
        Ok stream
    | Ok (`In_progress stream) ->
        (* Connection in progress, wait for writable - this suspends the process *)
        let source = Kernel.Net.Tcp_stream.to_source stream in
        Miniriot.syscall
        ~name:"TcpStream.connect"
        ~interest:Interest.writable
        ~source
        (fun () -> Ok stream)
    | Error _err ->
        (* Connection refused or error *)
        Error Connection_refused
  in
  connect_loop ()

let read = fun stream buffer ?(pos = 0) ?len ?timeout () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  (* Transform Time.Duration.t to float seconds for Miniriot.syscall *)
  let timeout = Option.map Time.Duration.to_secs_float timeout in
  let rec read_loop = fun () ->
    match Kernel.Net.Tcp_stream.read stream buffer ~pos ~len with
    | Ok 0 -> Error Closed
    | Ok bytes_read -> Ok bytes_read
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall
        ?timeout
        ~name:"TcpStream.read"
        ~interest:Interest.readable
        ~source
        (fun () -> read_loop ())
    | Error err -> Error (System_error err)
  in
  read_loop ()

let write = fun stream buffer ?(pos = 0) ?len () ->
  let len =
    match len with
    | None -> Bytes.length buffer - pos
    | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  let rec write_loop = fun () ->
    match Kernel.Net.Tcp_stream.write stream buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall
        ~name:"TcpStream.write"
        ~interest:Interest.writable
        ~source
        (fun () -> write_loop ())
    | Error err -> Error (System_error err)
  in
  write_loop ()

let close = Kernel.Net.Tcp_stream.close

let to_reader = fun stream ->
  let module Read = struct
    type nonrec t = t

    type nonrec err = error

    let read = fun t ?timeout:_ buf ->
      (* Note: timeout parameter ignored for now - Miniriot handles suspension *)
      read t buf ()

    let read_vectored = fun t bufs ->
      match Kernel.Net.Tcp_stream.read_vectored t bufs with
      | Ok n -> Ok n
      | Error err -> Error (System_error err)
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
      match Kernel.Net.Tcp_stream.write_vectored t bufs with
      | Ok n -> Ok n
      | Error err -> Error (System_error err)

    let flush = fun _t -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) stream
