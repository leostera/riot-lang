open Std
open Std.IO

type error = Error.t

type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string
  | Close of int option * string

type t = {
  stream : Net.TcpStream.t;
  uri : Net.Uri.t;
  mutable buffer : Buffer.t;
  mutable closed : bool;
}

let generate_websocket_key () =
  let random_bytes = Bytes.create 16 in
  for i = 0 to 15 do
    Bytes.set random_bytes i (Char.chr (Random.int 256))
  done;
  Data.Base64.encode_bytes random_bytes

let compute_accept_key key =
  let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let concat = key ^ magic in
  let hash = Crypto.Sha1.hash_string concat in
  let hash_bytes = Kernel.Crypto.Hash.to_bytes hash in
  Data.Base64.encode (Bytes.to_string hash_bytes)

let connect uri =
  let host = Net.Uri.host uri |> Option.unwrap_or ~default:"localhost" in
  let port = Net.Uri.port uri |> Option.unwrap_or ~default:80 in
  let path = Net.Uri.path uri in

  match Net.Addr.of_host_and_port ~host ~port with
  | Error (Net.Addr.System_error io_err) -> Error (Error.Net_error (Net.System_error io_err))
  | Error (Net.Addr.Invalid_port_number _ | Net.Addr.Invalid_format _) ->
      Error (Error.Net_error (Net.System_error IO.Invalid_argument))
  | Ok addr -> (
      match Net.TcpStream.connect addr with
      | Error Net.TcpStream.Closed -> Error (Error.Net_error Net.Closed)
      | Error Net.TcpStream.Connection_refused -> Error (Error.Net_error Net.Connection_refused)
      | Error (Net.TcpStream.System_error s) -> Error (Error.Net_error (Net.System_error s))
      | Ok stream -> (
          let key = generate_websocket_key () in
          let expected_accept = compute_accept_key key in

          let handshake =
            "GET " ^ path ^ " HTTP/1.1\r\n" ^
            "Host: " ^ host ^ "\r\n" ^
            "Upgrade: websocket\r\n" ^
            "Connection: Upgrade\r\n" ^
            "Sec-WebSocket-Key: " ^ key ^ "\r\n" ^
            "Sec-WebSocket-Version: 13\r\n" ^
            "\r\n"
          in

          let writer = Net.TcpStream.to_writer stream in
          match IO.write_all writer ~buf:handshake with
          | Error Net.TcpStream.Closed -> Error (Error.Net_error Net.Closed)
          | Error Net.TcpStream.Connection_refused -> Error (Error.Net_error Net.Connection_refused)
          | Error (Net.TcpStream.System_error s) -> Error (Error.Net_error (Net.System_error s))
          | Ok () -> (
              let reader = Net.TcpStream.to_reader stream in
              let buf = Bytes.create 4096 in
              let response_buffer = Buffer.create 1024 in

              let rec read_response () =
                match IO.read reader buf with
                | Error Net.TcpStream.Closed -> Error (Error.Net_error Net.Closed)
                | Error Net.TcpStream.Connection_refused -> Error (Error.Net_error Net.Connection_refused)
                | Error (Net.TcpStream.System_error s) -> Error (Error.Net_error (Net.System_error s))
                | Ok 0 ->
                    Error
                      (Error.Handshake_failed "Connection closed during handshake")
                | Ok n -> (
                    Buffer.add_subbytes response_buffer buf 0 n;
                    let response = Buffer.contents response_buffer in

                    match String.index_opt response '\r' with
                    | None -> read_response ()
                    | Some _ ->
                        if
                          String.contains response "\r"
                          && String.length response >= 4
                          && String.sub response (String.length response - 4) 4
                             = "\r\n\r\n"
                        then Ok response
                        else read_response ())
              in

              match read_response () with
              | Error e -> Error e
              | Ok response ->
                  let lines = String.split_on_char '\n' response in
                  let status_line = List.hd lines in

                  if
                    not
                      (String.contains status_line "1"
                      && String.contains status_line "0"
                      && String.contains status_line "1")
                  then
                    Error
                      (Error.Handshake_failed
                         "Server did not return 101 Switching Protocols")
                  else
                    let has_correct_accept =
                      List.exists
                        (fun line ->
                          let trimmed = String.trim line in
                          String.starts_with ~prefix:"Sec-WebSocket-Accept:"
                            trimmed
                          && String.contains trimmed ":"
                          &&
                          let parts = String.split_on_char ':' trimmed in
                          match parts with
                          | [ _; value ] -> String.trim value = expected_accept
                          | _ -> false)
                        lines
                    in

                    if not has_correct_accept then
                      Error
                        (Error.Handshake_failed "Invalid Sec-WebSocket-Accept header")
                    else
                      Ok
                        {
                          stream;
                          uri;
                          buffer = Buffer.create 4096;
                          closed = false;
                        })))

let send_frame conn frame =
  if conn.closed then Error Error.Closed
  else
    let serialized = Http.Ws.Serializer.serialize frame in
    let writer = Net.TcpStream.to_writer conn.stream in
    match IO.write_all writer ~buf:serialized with
    | Ok () -> Ok ()
    | Error Net.TcpStream.Closed -> Error (Error.Net_error Net.Closed)
    | Error Net.TcpStream.Connection_refused -> Error (Error.Net_error Net.Connection_refused)
    | Error (Net.TcpStream.System_error s) -> Error (Error.Net_error (Net.System_error s))

let send_text conn text =
  let frame = Http.Ws.Frame.text text in
  send_frame conn frame

let send_binary conn data =
  let frame = Http.Ws.Frame.binary data in
  send_frame conn frame

let send_ping conn ?(payload = "") () =
  let frame = Http.Ws.Frame.ping ~payload () in
  send_frame conn frame

let send_pong conn ?(payload = "") () =
  let frame = Http.Ws.Frame.pong ~payload () in
  send_frame conn frame

let send_close conn ?(code = 1000) ?(reason = "") () =
  let payload =
    let code_bytes = Bytes.create 2 in
    Bytes.set code_bytes 0 (Char.chr ((code lsr 8) land 0xFF));
    Bytes.set code_bytes 1 (Char.chr (code land 0xFF));
    Bytes.to_string code_bytes ^ reason
  in
  let frame = Http.Ws.Frame.close ~payload () in
  conn.closed <- true;
  send_frame conn frame

let receive conn =
  if conn.closed then Error Error.Closed
  else
    let rec try_parse () =
      let data = Buffer.contents conn.buffer in
      match Http.Ws.Parser.parse data with
      | Http.Ws.Parser.Done { value = frame; remaining } -> (
          Buffer.clear conn.buffer;
          Buffer.add_string conn.buffer remaining;

          match frame.Http.Ws.Frame.opcode with
          | Http.Ws.Frame.Text -> Ok (Text frame.payload)
          | Http.Ws.Frame.Binary -> Ok (Binary frame.payload)
          | Http.Ws.Frame.Ping ->
              let _ = send_pong conn ~payload:frame.payload () in
              Ok (Ping frame.payload)
          | Http.Ws.Frame.Pong -> Ok (Pong frame.payload)
          | Http.Ws.Frame.Close ->
              conn.closed <- true;
              if String.length frame.payload >= 2 then
                let code =
                  (Char.code frame.payload.[0] lsl 8)
                  lor Char.code frame.payload.[1]
                in
                let reason =
                  if String.length frame.payload > 2 then
                    String.sub frame.payload 2 (String.length frame.payload - 2)
                  else ""
                in
                Ok (Close (Some code, reason))
              else Ok (Close (None, ""))
          | Http.Ws.Frame.Continuation -> Error Error.Invalid_frame)
      | Http.Ws.Parser.Need_more -> (
          let reader = Net.TcpStream.to_reader conn.stream in
          let buf = Bytes.create 4096 in
          match IO.read reader buf with
          | Error Net.TcpStream.Closed -> Error (Error.Net_error Net.Closed)
          | Error Net.TcpStream.Connection_refused -> Error (Error.Net_error Net.Connection_refused)
          | Error (Net.TcpStream.System_error s) -> Error (Error.Net_error (Net.System_error s))
          | Ok 0 -> Error Error.Eof
          | Ok n ->
              Buffer.add_subbytes conn.buffer buf 0 n;
              try_parse ())
      | Http.Ws.Parser.Error msg -> Error (Error.Handshake_failed msg)
    in
    try_parse ()

let close conn =
  (if not conn.closed then
     let _ = send_close conn () in
     ());
  Net.TcpStream.close conn.stream
