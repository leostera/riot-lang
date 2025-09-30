(** Network I/O operations for Miniriot *)

open Kernel.IO

type error = [ `Connection_refused | `Closed | `System_error of string ]

module Uri = Uri

module Addr = struct
  include Kernel.IO.Net.Addr

  (* Wrap of_host_and_port to match our error type *)
  let of_host_and_port ~host ~port =
    match Kernel.IO.Net.Addr.of_host_and_port ~host ~port with
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
  type t = Kernel.IO.Net.TcpListener.t

  let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
    match
      Kernel.IO.Net.TcpListener.bind ~reuse_addr ~reuse_port ~backlog addr
    with
    | Ok t -> Ok t
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `Unix_error _ | `Would_block ) ->
        Error (`System_error "Failed to bind")

  let accept t =
    let source = Kernel.IO.Net.TcpListener.to_source t in
    let rec accept_loop () =
      match Kernel.IO.Net.TcpListener.accept t with
      | Ok (stream, addr) -> Ok (stream, addr)
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Miniriot.syscall ~name:"TcpListener.accept"
            ~interest:Kernel.IO.Interest.readable ~source (fun () ->
              accept_loop ())
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ ) ->
          (* Some other error *)
          Error (`System_error "Accept failed")
    in
    accept_loop ()

  let close = Kernel.IO.Net.TcpListener.close
end

module TcpStream = struct
  type t = Kernel.IO.Net.TcpStream.t

  let connect addr =
    let rec connect_loop () =
      match Kernel.IO.Net.TcpStream.connect addr with
      | Ok (`Connected stream) -> Ok stream
      | Ok (`In_progress stream) ->
          (* Connection in progress, wait for writable - this suspends the process *)
          let source = Kernel.IO.Net.TcpStream.to_source stream in
          Miniriot.syscall ~name:"TcpStream.connect"
            ~interest:Kernel.IO.Interest.writable ~source (fun () -> Ok stream)
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
    let source = Kernel.IO.Net.TcpStream.to_source stream in
    let rec read_loop () =
      match Kernel.IO.Net.TcpStream.read stream buffer ~pos ~len with
      | Ok 0 -> Error `Closed (* EOF *)
      | Ok bytes_read -> Ok bytes_read
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Miniriot.syscall ~name:"TcpStream.read"
            ~interest:Kernel.IO.Interest.readable ~source (fun () ->
              read_loop ())
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
    let source = Kernel.IO.Net.TcpStream.to_source stream in
    let rec write_loop () =
      match Kernel.IO.Net.TcpStream.write stream buffer ~pos ~len with
      | Ok bytes_written -> Ok bytes_written
      | Error `Would_block ->
          (* Would block, register interest and wait - this suspends the process *)
          Miniriot.syscall ~name:"TcpStream.write"
            ~interest:Kernel.IO.Interest.writable ~source (fun () ->
              write_loop ())
      | Error
          ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
          | `Process_down | `Timeout | `Unix_error _ ) ->
          (* Some other error *)
          Error (`System_error "Write failed")
    in
    write_loop ()

  let close = Kernel.IO.Net.TcpStream.close
end

module TcpServer = struct
  (** TCP server that manages the listener and provides accept functionality *)

  type handler = req:string -> TcpStream.t -> unit
  (** Handler receives request string and stream for responses *)

  type t = { listener : TcpListener.t; handler : handler }

  let read_line stream =
    let buffer = Bytes.create 4096 in
    let rec loop acc =
      match TcpStream.read stream buffer () with
      | Error _ -> Error "Failed to read from stream"
      | Ok 0 -> Error "Connection closed"
      | Ok n -> (
          let data = Bytes.sub_string buffer 0 n in
          let combined = acc ^ data in
          (* Look for newline *)
          match String.index_opt combined '\n' with
          | Some idx ->
              let line = String.sub combined 0 idx in
              Ok line
          | None -> loop combined)
    in
    loop ""

  let rec accept_loop t =
    match TcpListener.accept t.listener with
    | Error e -> Error e
    | Ok (stream, _client_addr) ->
        let _connection_pid =
          Miniriot.spawn (fun () ->
              (* Read lines in a loop using the read_line helper *)
              let rec connection_loop () =
                match read_line stream with
                | Ok req ->
                    (* Call handler with request string and stream *)
                    t.handler ~req stream;
                    connection_loop ()
                | Error _ ->
                    (* Connection closed, clean up *)
                    TcpStream.close stream;
                    Ok ()
              in
              connection_loop ())
        in
        accept_loop t

  let listen ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr
      ~handler =
    match TcpListener.bind ~reuse_addr ~reuse_port ~backlog addr with
    | Error e -> Error e
    | Ok listener ->
        Fun.protect
          ~finally:(fun () -> TcpListener.close listener)
          (fun () -> accept_loop { listener; handler })
end

module TcpClient = struct
  type t = {
    stream : TcpStream.t;
    mutable leftover : string; (* Buffer for data read past newline *)
  }

  let connect ~host ~port =
    match Addr.of_host_and_port ~host ~port with
    | Error e -> Error e
    | Ok addr -> (
        match TcpStream.connect addr with
        | Ok stream -> Ok { stream; leftover = "" }
        | Error e -> Error e)

  let send t data =
    let buffer = Bytes.of_string data in
    let len = Bytes.length buffer in
    let rec send_all pos =
      if pos >= len then Ok ()
      else
        match TcpStream.write t.stream buffer ~pos ~len:(len - pos) () with
        | Ok bytes_written -> send_all (pos + bytes_written)
        | Error e ->
            Error
              (Printf.sprintf "Send failed: %s"
                 (match e with
                 | `Closed -> "connection closed"
                 | `System_error s -> s))
    in
    send_all 0

  let receive t =
    (* Check if we already have a complete line in leftover buffer *)
    match String.index_opt t.leftover '\n' with
    | Some idx ->
        (* Found newline in leftover, return line and save remainder *)
        let line = String.sub t.leftover 0 idx in
        let remainder_start = idx + 1 in
        let remainder_len = String.length t.leftover - remainder_start in
        t.leftover <-
          (if remainder_len > 0 then
             String.sub t.leftover remainder_start remainder_len
           else "");
        Ok line
    | None ->
        (* No complete line in leftover, need to read more *)
        let buffer = Bytes.create 4096 in
        let buffer_size = Bytes.length buffer in

        (* Read until we get a newline *)
        let rec read_line acc =
          match TcpStream.read t.stream buffer ~pos:0 ~len:buffer_size () with
          | Ok bytes_read -> (
              let data = Bytes.sub_string buffer 0 bytes_read in
              let full_data = acc ^ data in
              (* Check if we have a complete line *)
              match String.index_opt full_data '\n' with
              | Some idx ->
                  (* Found newline, save remainder and return line *)
                  let line = String.sub full_data 0 idx in
                  let remainder_start = idx + 1 in
                  let remainder_len =
                    String.length full_data - remainder_start
                  in
                  t.leftover <-
                    (if remainder_len > 0 then
                       String.sub full_data remainder_start remainder_len
                     else "");
                  Ok line
              | None ->
                  (* No newline yet, keep reading *)
                  read_line full_data)
          | Error `Closed ->
              if acc = "" && t.leftover = "" then Error "Connection closed"
              else
                (* Return what we have, clear leftover *)
                let result = t.leftover ^ acc in
                t.leftover <- "";
                Ok result
          | Error (`System_error s) -> Error s
        in
        read_line t.leftover

  let close t = TcpStream.close t.stream
end
