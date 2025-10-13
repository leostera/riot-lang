(** TCP stream for connected sockets *)

open Global
open Kernel.Async

type t = Kernel.Net.Tcp_stream.t
type error = [ `Connection_refused | `Closed | `System_error of string ]

let connect addr =
  let rec connect_loop () =
    match Kernel.Net.Tcp_stream.connect addr with
    | Ok (`Connected stream) -> Ok stream
    | Ok (`In_progress stream) ->
        (* Connection in progress, wait for writable - this suspends the process *)
        let source = Kernel.Net.Tcp_stream.to_source stream in
        Miniriot.syscall ~name:"TcpStream.connect" ~interest:Interest.writable
          ~source (fun () -> Ok stream)
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `IO_error _ | `Would_block ) ->
        (* Connection refused or error *)
        Error `Connection_refused
  in
  connect_loop ()

let read stream buffer ?(pos = 0) ?len () =
  let len =
    match len with None -> Bytes.length buffer - pos | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  let rec read_loop () =
    match Kernel.Net.Tcp_stream.read stream buffer ~pos ~len with
    | Ok 0 -> Error `Closed (* EOF *)
    | Ok bytes_read -> Ok bytes_read
    | Error `Would_block ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall ~name:"TcpStream.read" ~interest:Interest.readable
          ~source (fun () -> read_loop ())
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `IO_error _ ) ->
        (* Some other error *)
        Error (`System_error "Read failed")
  in
  read_loop ()

let write stream buffer ?(pos = 0) ?len () =
  let len =
    match len with None -> Bytes.length buffer - pos | Some l -> l
  in
  let source = Kernel.Net.Tcp_stream.to_source stream in
  let rec write_loop () =
    match Kernel.Net.Tcp_stream.write stream buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error `Would_block ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall ~name:"TcpStream.write" ~interest:Interest.writable
          ~source (fun () -> write_loop ())
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `IO_error _ ) ->
        (* Some other error *)
        Error (`System_error "Write failed")
  in
  write_loop ()

let close = Kernel.Net.Tcp_stream.close
