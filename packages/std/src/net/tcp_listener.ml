(** TCP listener for accepting connections *)
open Global
open Kernel.Async

type t = Kernel.Net.TcpListener.t

type error =
  | Connection_refused
  | Closed
  | System_error of IO.error

let io_error_of_listener_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Net.TcpListener.InvalidBacklog _ -> IO.Invalid_argument
  | Kernel.Net.TcpListener.InvalidSocketAddr _ -> IO.Invalid_argument
  | Kernel.Net.TcpListener.WouldBlock -> IO.Operation_would_block
  | Kernel.Net.TcpListener.AddressInUse -> IO.Address_already_in_use
  | Kernel.Net.TcpListener.AddressNotAvailable -> IO.Cannot_assign_requested_address
  | Kernel.Net.TcpListener.ConnectionAborted -> IO.Software_caused_connection_abort
  | Kernel.Net.TcpListener.System error -> IO.from_system_error error

let bind = fun ?(reuse_addr = true) ?(reuse_port = false) ?(backlog = 128) addr ->
  match Kernel.Net.TcpListener.bind ~reuse_addr ~reuse_port ~backlog addr with
  | Ok t -> Ok t
  | Error err -> Error (System_error (io_error_of_listener_error err))

let accept = fun ?timeout t ->
  let source = Kernel.Net.TcpListener.to_source t in
  let timeout_secs = Option.map timeout ~fn:Time.Duration.to_secs_float in
  let rec accept_loop () =
    match Kernel.Net.TcpListener.accept t with
    | Ok (stream, addr) -> Ok (stream, addr)
    | Error Kernel.Net.TcpListener.WouldBlock ->
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
  match Kernel.Net.TcpListener.local_addr t with
  | Ok addr -> addr
  | Error err ->
      panic
        (format
          Format.[
            str "TcpListener.local_addr failed: ";
            str (IO.error_message (io_error_of_listener_error err));
          ])

let close = fun t ->
  match Kernel.Net.TcpListener.close t with
  | Ok () -> ()
  | Error _ -> ()
