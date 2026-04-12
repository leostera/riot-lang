(** TCP listener for accepting connections *)
open Global
open Kernel.Async

type t = Kernel.Net.Tcp_listener.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let io_error_of_listener_error = function
  | Kernel.Net.Tcp_listener.InvalidBacklog _ -> IO.Invalid_argument
  | Kernel.Net.Tcp_listener.InvalidSocketAddr _ -> IO.Invalid_argument
  | Kernel.Net.Tcp_listener.WouldBlock -> IO.Operation_would_block
  | Kernel.Net.Tcp_listener.AddressInUse -> IO.Address_already_in_use
  | Kernel.Net.Tcp_listener.AddressNotAvailable -> IO.Cannot_assign_requested_address
  | Kernel.Net.Tcp_listener.ConnectionAborted -> IO.Software_caused_connection_abort
  | Kernel.Net.Tcp_listener.System error -> IO.of_system_error error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ->
  match Kernel.Net.Tcp_listener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Ok t -> Ok t
  | Error err -> Error (System_error (io_error_of_listener_error err))

let accept = fun ?timeout t ->
  let source = Kernel.Net.Tcp_listener.to_source t in
  let timeout_secs = Option.map Time.Duration.to_secs_float timeout in
  let rec accept_loop () =
    match Kernel.Net.Tcp_listener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error Kernel.Net.Tcp_listener.WouldBlock ->
        Runtime.syscall
          ?timeout:timeout_secs
          ~name:"TcpListener.accept"
          ~interest:Interest.readable
          ~source
          (fun () -> accept_loop ())
    | Error err -> Error (System_error (io_error_of_listener_error err))
  in
  accept_loop ()

let local_addr = fun t ->
  match Kernel.Net.Tcp_listener.local_addr t with
  | Ok addr -> addr
  | Error err -> panic
    (format Format.[ str "TcpListener.local_addr failed: "; str (IO.error_message (io_error_of_listener_error err)) ])

let close = fun t ->
  ignore (Kernel.Net.Tcp_listener.close t)
