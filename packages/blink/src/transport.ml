open Std

(* Result monad for cleaner error handling *)

let ( let* ) = Result.and_then

module type Intf = sig
  val name: string

  val connect: Net.Addr.stream_addr -> Net.Uri.t -> (Connection.t, Error.t) result
end

module Tcp: Intf = struct
  let name = "tcp"

  let connect = fun addr uri ->
    match Net.TcpStream.connect addr with
    | Error Net.TcpStream.Closed -> Error (Error.NetError Net.Closed)
    | Error Net.TcpStream.Connection_refused -> Error (Error.NetError Net.Connection_refused)
    | Error (Net.TcpStream.System_error error) -> Error (Error.NetError (Net.System_error error))
    | Ok sock ->
        let reader = Net.TcpStream.to_reader sock in
        let writer = Net.TcpStream.to_writer sock in
        Ok (Connection.make ~reader ~writer ~from_io_error:Error.from_io_error ~uri)
end

module Tls: Intf = struct
  let name = "tls"

  let connect = fun addr uri ->
    match Net.TcpStream.connect addr with
    | Error Net.TcpStream.Closed -> Error (Error.NetError Net.Closed)
    | Error Net.TcpStream.Connection_refused -> Error (Error.NetError Net.Connection_refused)
    | Error (Net.TcpStream.System_error s) -> Error (Error.NetError (Net.System_error s))
    | Ok sock ->
        let hostname =
          Net.Uri.host uri
          |> Option.unwrap_or ~default:"localhost"
        in
        match Net.TlsStream.from_tcp_client ~hostname sock with
        | Error e -> Error (Error.TlsError e)
        | Ok tls ->
            let reader = Net.TlsStream.to_reader tls in
            let writer = Net.TlsStream.to_writer tls in
            Ok (Connection.make ~reader ~writer ~from_io_error:Error.from_io_error ~uri)
end

let connect = fun uri ->
  let host =
    Net.Uri.host uri
    |> Option.unwrap_or ~default:"localhost"
  in
  let default_port =
    match Net.Uri.scheme uri with
    | Some "https" -> 443
    | _ -> 80
  in
  let port =
    Net.Uri.port uri
    |> Option.unwrap_or ~default:default_port
  in
  Log.info "connecting!";
  match Net.Addr.from_host_and_port ~host ~port with
  | Error (Net.Addr.System_error io_err) -> Error (Error.NetError (Net.System_error io_err))
  | Error (Net.Addr.Invalid_port_number _ | Net.Addr.Invalid_format _) ->
      Error (Error.NetError (Net.System_error IO.Invalid_argument))
  | Ok addr ->
      match Net.Uri.scheme uri with
      | Some "https"
      | Some "wss" -> Tls.connect addr uri
      | Some "http"
      | Some "ws"
      | None -> Tcp.connect addr uri
      | Some _ -> Tcp.connect addr uri
