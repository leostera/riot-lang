(** TCP listener for accepting connections *)

open Global
open Kernel.Async

type t = Kernel.Net.Tcp_listener.t

type error =
  | Connection_refused
  | Closed
  | System_error of string

let bind ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr =
  match Kernel.Net.Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Ok t -> Ok t
  | Error err -> Error (System_error (IO.error_message err))

let accept t =
  let source = Kernel.Net.Tcp_listener.to_source t in
  let rec accept_loop () =
    match Kernel.Net.Tcp_listener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error IO.Operation_would_block ->
        (* Would block, register interest and wait - this suspends the process *)
        Miniriot.syscall ~name:"TcpListener.accept" ~interest:Interest.readable
          ~source (fun () -> accept_loop ())
    | Error err ->
        (* Some other error *)
        Error (System_error (IO.error_message err))
  in
  accept_loop ()

let local_addr t =
  match Kernel.Net.Tcp_listener.local_addr t with
  | Ok addr -> addr
  | Error err -> panic ("TcpListener.local_addr failed: " ^ IO.error_message err)

let close = Kernel.Net.Tcp_listener.close
