(** TCP listener for accepting connections *)
open Global
open Kernel.Async

type t = Kernel.Net.Tcp_listener.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ->
  match Kernel.Net.Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Ok t -> Ok t
  | Error err -> Error (System_error err)

let accept = fun ?timeout t ->
  let source = Kernel.Net.Tcp_listener.to_source t in
  let timeout_secs = Option.map Time.Duration.to_secs_float timeout in
  let rec accept_loop () =
    match Kernel.Net.Tcp_listener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error IO.Operation_would_block
    | Error IO.Resource_unavailable_try_again ->
        (* Would block / EAGAIN / EWOULDBLOCK - register interest and wait *)
        Runtime.syscall
          ?timeout:timeout_secs
          ~name:"TcpListener.accept"
          ~interest:Interest.readable
          ~source
          (fun () -> accept_loop ())
    | Error err ->
        (* Some other error - EINTR is already handled by kernel layer *)
        Error (System_error err)
  in
  accept_loop ()

let local_addr = fun t ->
  match Kernel.Net.Tcp_listener.local_addr t with
  | Ok addr -> addr
  | Error err -> panic
    (format Format.[ str "TcpListener.local_addr failed: "; str (IO.error_message err) ])

let close = Kernel.Net.Tcp_listener.close
