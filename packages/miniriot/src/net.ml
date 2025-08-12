(** Network I/O operations for Miniriot *)

open Gluon

type error = [ `Connection_refused | `Closed | `System_error of string ]

module Addr = struct
  include Gluon.Net.Addr
  
  (* Override parse to convert error type *)
  let parse s =
    match Gluon.Net.Addr.parse s with
    | Ok addr -> Ok addr
    | Error `Noop -> Error (`System_error "Invalid address format")
end

module TcpListener = struct
  type t = Gluon.Net.TcpListener.t

  let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
    match Gluon.Net.TcpListener.bind ~reuse_addr ~reuse_port ~backlog addr with
    | Ok t -> Ok t
    | Error `Noop -> Error (`System_error "Failed to bind")

  let accept t =
    let source = Gluon.Net.TcpListener.to_source t in
    (* First attempt to accept *)
    match Gluon.Net.TcpListener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error `Noop ->
        (* Would block, register interest and wait - this suspends the process *)
        Effects.syscall "TcpListener.accept" Interest.readable source (fun () ->
            (* When we resume, the socket should be ready - try accepting again *)
            match Gluon.Net.TcpListener.accept t with
            | Ok (stream, addr) -> Ok (stream, addr)
            | Error `Noop -> Error (`System_error "Accept failed after being ready")
        )

  let close = Gluon.Net.TcpListener.close
end

module TcpStream = struct
  type t = Gluon.Net.TcpStream.t

  let connect addr =
    match Gluon.Net.TcpStream.connect addr with
    | Ok (`Connected stream) -> Ok stream
    | Ok (`In_progress stream) ->
        (* Connection in progress, wait for writable - this suspends the process *)
        let source = Gluon.Net.TcpStream.to_source stream in
        Effects.syscall "TcpStream.connect" Interest.writable source
          (fun () -> Ok stream)
    | Error `Noop ->
        (* Connection refused or error *)
        Error (`Connection_refused)

  let read stream buffer ?(pos = 0) ?len () =
    let len = match len with None -> Bytes.length buffer - pos | Some l -> l in
    let source = Gluon.Net.TcpStream.to_source stream in
    match Gluon.Net.TcpStream.read stream buffer ~pos ~len with
    | Ok 0 -> Error `Closed  (* EOF *)
    | Ok bytes_read -> Ok bytes_read
    | Error `Noop ->
        (* Would block, register interest and wait - this suspends the process *)
        Effects.syscall "TcpStream.read" Interest.readable source (fun () ->
            match Gluon.Net.TcpStream.read stream buffer ~pos ~len with
            | Ok 0 -> Error `Closed  (* EOF *)
            | Ok bytes_read -> Ok bytes_read
            | Error `Noop -> Error (`System_error "Read failed after being ready")
        )

  let write stream buffer ?(pos = 0) ?len () =
    let len = match len with None -> Bytes.length buffer - pos | Some l -> l in
    let source = Gluon.Net.TcpStream.to_source stream in
    match Gluon.Net.TcpStream.write stream buffer ~pos ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error `Noop ->
        (* Would block, register interest and wait - this suspends the process *)
        Effects.syscall "TcpStream.write" Interest.writable source (fun () ->
            match Gluon.Net.TcpStream.write stream buffer ~pos ~len with
            | Ok bytes_written -> Ok bytes_written
            | Error `Noop -> Error (`System_error "Write failed after being ready")
        )

  let close = Gluon.Net.TcpStream.close
end
