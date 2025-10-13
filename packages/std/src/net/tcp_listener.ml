(** TCP listener for accepting connections *)

open Global
open Kernel.Async

type t = Kernel.Net.Tcp_listener.t
type error = [ `Connection_refused | `Closed | `System_error of string ]

let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
  match
    Kernel.Net.Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr
  with
  | Ok t -> Ok t
  | Error
      ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
      | `Process_down | `Timeout | `IO_error _ | `Would_block ) ->
      Error (`System_error "Failed to bind")

let accept t =
  let source = Kernel.Net.Tcp_listener.to_source t in
  let rec accept_loop () =
    match Kernel.Net.Tcp_listener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error `Would_block ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall ~name:"TcpListener.accept"
          ~interest:Interest.readable ~source (fun () -> accept_loop ())
    | Error
        ( `Noop | `Closed | `Connection_closed | `Eof | `Exn _ | `No_info
        | `Process_down | `Timeout | `IO_error _ ) ->
        (* Some other error *)
        Error (`System_error "Accept failed")
  in
  accept_loop ()

let close = Kernel.Net.Tcp_listener.close
