open Std

(* Result monad for cleaner error handling *)

let ( let* ) = Result.and_then

module type Intf = sig
  val name: string

  val connect:
    ?read_timeout:Time.Duration.t ->
    Net.Addr.stream_addr ->
    Net.Uri.t ->
    (Connection.t, Error.t) result
end

let tcp_error_to_io_error = fun error ->
  match error with
  | Net.TcpStream.Closed -> IO.Closed
  | Net.TcpStream.Connection_refused -> IO.Connection_refused
  | Net.TcpStream.System_error error -> error

let tcp_reader ?read_timeout sock =
  match read_timeout with
  | None -> Net.TcpStream.to_reader sock
  | Some timeout ->
      let max_read_size = 64 * 1_024 in
      let module Read = struct
        type t = Net.TcpStream.t

        let writable into =
          if IO.Buffer.writable_bytes into = 0 then (
            match IO.Buffer.ensure_free into 4_096 with
            | Ok () -> IO.Buffer.writable into
            | Error _ -> panic "Blink.Transport.tcp_reader.ensure_free failed"
          ) else
            IO.Buffer.writable into

        let read t ~into =
          let writable = writable into in
          let scratch_len = Int.min max_read_size (IO.IoSlice.length writable) in
          let scratch = IO.Bytes.create ~size:scratch_len in
          match Net.TcpStream.read t scratch ~timeout () with
          | Ok count ->
              let chunk = IO.Bytes.sub_unchecked scratch ~offset:0 ~len:count in
              (
                match IO.Buffer.append_bytes into chunk with
                | Ok () -> Ok count
                | Error _ -> panic "Blink.Transport.tcp_reader.append failed"
              )
          | Error error -> Error (tcp_error_to_io_error error)

        let read_vectored t ~into =
          let total = IO.IoVec.length into in
          if total = 0 then
            Ok 0
          else
            let scratch_len = Int.min max_read_size total in
            let scratch = IO.Bytes.create ~size:scratch_len in
            match Net.TcpStream.read t scratch ~timeout () with
            | Error error -> Error (tcp_error_to_io_error error)
            | Ok count ->
                let copied = ref 0 in
                IO.IoVec.for_each
                  ~fn:(fun segment ->
                    if !copied < count then (
                      let remaining = count - !copied in
                      let available = IO.IoSlice.length segment in
                      let chunk_len = Int.min remaining available in
                      if chunk_len > 0 then
                        IO.IoSlice.blit_from_bytes_unchecked
                          scratch
                          ~src_off:!copied
                          segment
                          ~dst_off:0
                          ~len:chunk_len
                    );
                    copied := !copied + IO.IoSlice.length segment)
                  into;
                Ok count

        let is_read_vectored _ = true
      end in
      IO.Reader.from_source (module Read) sock

module Tcp: Intf = struct
  let name = "tcp"

  let connect = fun ?read_timeout addr uri ->
    match Net.TcpStream.connect addr with
    | Error Net.TcpStream.Closed -> Error (Error.NetError Net.Closed)
    | Error Net.TcpStream.Connection_refused -> Error (Error.NetError Net.Connection_refused)
    | Error (Net.TcpStream.System_error error) -> Error (Error.NetError (Net.System_error error))
    | Ok sock ->
        let reader = tcp_reader ?read_timeout sock in
        let writer = Net.TcpStream.to_writer sock in
        Ok (Connection.make ~reader ~writer ~on_close:(fun () -> Net.TcpStream.close sock) ~uri ())
end

module Tls: Intf = struct
  let name = "tls"

  let connect = fun ?read_timeout addr uri ->
    match Net.TcpStream.connect addr with
    | Error Net.TcpStream.Closed -> Error (Error.NetError Net.Closed)
    | Error Net.TcpStream.Connection_refused -> Error (Error.NetError Net.Connection_refused)
    | Error (Net.TcpStream.System_error s) -> Error (Error.NetError (Net.System_error s))
    | Ok sock ->
        let hostname =
          Net.Uri.host uri
          |> Option.unwrap_or ~default:"localhost"
        in
        let reader = tcp_reader ?read_timeout sock in
        let writer = Net.TcpStream.to_writer sock in
        match Net.TlsStream.from_client_io ~reader ~writer ~hostname () with
        | Error e -> Error (Error.TlsError e)
        | Ok tls ->
            let reader = Net.TlsStream.to_reader tls in
            let writer = Net.TlsStream.to_writer tls in
            Ok (Connection.make
              ~reader
              ~writer
              ~on_close:(fun () -> Net.TcpStream.close sock)
              ~uri
              ())
end

let connect = fun ?read_timeout uri ->
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
  match Net.Addr.from_host_and_port ~host ~port with
  | Error (Net.Addr.System_error io_err) -> Error (Error.NetError (Net.System_error io_err))
  | Error (Net.Addr.Invalid_port_number _ | Net.Addr.Invalid_format _) ->
      Error (Error.NetError (Net.System_error IO.Invalid_argument))
  | Ok addr ->
      match Net.Uri.scheme uri with
      | Some "https"
      | Some "wss" -> Tls.connect ?read_timeout addr uri
      | Some "http"
      | Some "ws"
      | None -> Tcp.connect ?read_timeout addr uri
      | Some _ -> Tcp.connect ?read_timeout addr uri
