(** Network I/O operations for Miniriot *)

open Gluon

type error = [ `Connection_refused | `Closed | `System_error of string ]

module Addr = struct
  include Gluon.Net.Addr

  (* Wrap of_host_and_port to match our error type *)
  let of_host_and_port ~host ~port =
    match Gluon.Net.Addr.of_host_and_port ~host ~port with
    | Ok addr -> Ok addr
    | Error `Noop -> Error (`System_error "Failed to resolve address")
    | Error `No_info -> Error (`System_error "No address info available")
    | Error _ -> Error (`System_error "Address resolution error")

  (* Implement parse using available functions *)
  let parse s =
    (* Try to parse host:port format *)
    match String.rindex_opt s ':' with
    | None -> Error (`System_error "Invalid address format: missing port")
    | Some idx -> (
        let host = String.sub s 0 idx in
        let port_str = String.sub s (idx + 1) (String.length s - idx - 1) in
        match int_of_string_opt port_str with
        | None -> Error (`System_error "Invalid port number")
        | Some port -> (
            match of_host_and_port ~host ~port with
            | Ok addr -> Ok addr
            | Error `Noop -> Error (`System_error "Failed to parse address")
            | Error `No_info ->
                Error (`System_error "No address info available")
            | Error _ -> Error (`System_error "Address parsing error")))
end

module TcpListener = struct
  type t = Gluon.Net.TcpListener.t

  let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
    match Gluon.Net.TcpListener.bind ~reuse_addr ~reuse_port ~backlog addr with
    | Ok t -> Ok t
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `Unix_error _ | `Would_block ) ->
        Error (`System_error "Failed to bind")

  let accept t =
    let source = Gluon.Net.TcpListener.to_source t in
    let rec accept_loop () =
      match Gluon.Net.TcpListener.accept t with
      | Ok (stream, addr) -> Ok (stream, addr)
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Effects.syscall ~name:"TcpListener.accept" ~interest:Interest.readable
            ~source (fun () -> accept_loop ())
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ ) ->
          (* Some other error *)
          Error (`System_error "Accept failed")
    in
    accept_loop ()

  let close = Gluon.Net.TcpListener.close
end

module TcpStream = struct
  type t = Gluon.Net.TcpStream.t

  let connect addr =
    let rec connect_loop () =
      match Gluon.Net.TcpStream.connect addr with
      | Ok (`Connected stream) -> Ok stream
      | Ok (`In_progress stream) ->
          (* Connection in progress, wait for writable - this suspends the process *)
          let source = Gluon.Net.TcpStream.to_source stream in
          Effects.syscall ~name:"TcpStream.connect" ~interest:Interest.writable
            ~source (fun () -> Ok stream)
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ | `Would_block ) ->
          (* Connection refused or error *)
          Error `Connection_refused
    in
    connect_loop ()

  let read stream buffer ?(pos = 0) ?len () =
    let len =
      match len with None -> Bytes.length buffer - pos | Some l -> l
    in
    let source = Gluon.Net.TcpStream.to_source stream in
    let rec read_loop () =
      match Gluon.Net.TcpStream.read stream buffer ~pos ~len with
      | Ok 0 -> Error `Closed (* EOF *)
      | Ok bytes_read -> Ok bytes_read
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Effects.syscall ~name:"TcpStream.read" ~interest:Interest.readable
            ~source (fun () -> read_loop ())
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ ) ->
          (* Some other error *)
          Error (`System_error "Read failed")
    in
    read_loop ()

  let write stream buffer ?(pos = 0) ?len () =
    let len =
      match len with None -> Bytes.length buffer - pos | Some l -> l
    in
    let source = Gluon.Net.TcpStream.to_source stream in
    let rec write_loop () =
      match Gluon.Net.TcpStream.write stream buffer ~pos ~len with
      | Ok bytes_written -> Ok bytes_written
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Effects.syscall ~name:"TcpStream.write" ~interest:Interest.writable
            ~source (fun () -> write_loop ())
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ ) ->
          (* Some other error *)
          Error (`System_error "Write failed")
    in
    write_loop ()

  let close = Gluon.Net.TcpStream.close
end
