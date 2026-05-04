open Std

module Buffer = IO.Buffer

type error = Error.t

type message =
  | Text of string
  | Binary of string
  | Ping of string
  | Pong of string
  | Close of int option * string

type transport =
  | Plain of Net.TcpStream.t
  | Secure of Net.TcpStream.t * Net.TcpStream.t Net.TlsStream.t

type t = {
  transport: transport;
  uri: Net.Uri.t;
  reader: IO.Reader.t;
  writer: IO.Writer.t;
  mutable buffer: Buffer.t;
  mutable closed: bool;
}

let generate_websocket_key = fun () ->
  let random_bytes = IO.Bytes.create ~size:16 in
  for i = 0 to 15 do
    yield ();
    IO.Bytes.set_unchecked
      random_bytes
      ~at:i
      ~char:(
        Char.from_int_unchecked
          (
            Random.int 256
            |> Result.expect ~msg:"failed to generate websocket key byte"
          )
      )
  done;
  Encoding.Base64.encode_bytes random_bytes

let compute_accept_key = fun key ->
  let magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" in
  let concat = key ^ magic in
  let hash = Crypto.Sha1.hash_string concat in
  let hash_bytes = Crypto.Hash.to_bytes hash in
  Encoding.Base64.encode (IO.Bytes.to_string hash_bytes)

let close_transport = fun transport ->
  match transport with
  | Plain stream -> Net.TcpStream.close stream
  | Secure (stream, tls) ->
      Net.TlsStream.close tls;
      Net.TcpStream.close stream

let connect_transport = fun uri host ->
  let scheme =
    Net.Uri.scheme uri
    |> Option.unwrap_or ~default:"ws"
  in
  let default_port =
    match scheme with
    | "wss" -> 443
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
  | Ok addr -> (
      match Net.TcpStream.connect addr with
      | Error Net.TcpStream.Closed -> Error (Error.NetError Net.Closed)
      | Error Net.TcpStream.Connection_refused -> Error (Error.NetError Net.Connection_refused)
      | Error (Net.TcpStream.System_error error) -> Error (Error.NetError (Net.System_error error))
      | Ok stream -> (
          match scheme with
          | "ws" ->
              Ok (Plain stream, Net.TcpStream.to_reader stream, Net.TcpStream.to_writer stream)
          | "wss" -> (
              match Net.TlsStream.from_tcp_client ~hostname:host stream with
              | Error error ->
                  Net.TcpStream.close stream;
                  Error (Error.TlsError error)
              | Ok tls ->
                  Ok (
                    Secure (stream, tls),
                    Net.TlsStream.to_reader tls,
                    Net.TlsStream.to_writer tls
                  )
            )
          | _ ->
              Net.TcpStream.close stream;
              Error (Error.ProtocolError ("unsupported websocket scheme: " ^ scheme))
        )
    )

let host_header = fun uri host ->
  match Net.Uri.port uri with
  | Some port -> host ^ ":" ^ Int.to_string port
  | None -> host

let write_all = fun conn text ->
  match IO.write_all conn.writer ~from:(IO.Buffer.from_string text) with
  | Ok () -> Ok ()
  | Error error -> Error (Error.from_io_error error)

let read_handshake_response = fun conn ->
  let response_buffer = Buffer.create ~size:1_024 in
  let rec read_response () =
    let chunk = IO.Buffer.create ~size:4_096 in
    match IO.read conn.reader ~into:chunk with
    | Error error -> Error (Error.from_io_error error)
    | Ok 0 -> Error (Error.HandshakeFailed "Connection closed during handshake")
    | Ok _ ->
        let readable = IO.Buffer.readable chunk in
        let _ =
          Buffer.append_slice response_buffer readable
          |> Result.expect ~msg:"failed to append websocket handshake bytes"
        in
        let response = Buffer.contents response_buffer in
        if String.contains response "\r\n\r\n" then
          Ok response
        else
          read_response ()
  in
  read_response ()

let validate_handshake = fun expected_accept response ->
  let lines = String.split ~by:"\n" response in
  let status_line =
    List.head lines
    |> Option.unwrap_or ~default:""
    |> String.trim
  in
  if not (String.contains status_line " 101 ") then
    Error (Error.HandshakeFailed "Server did not return 101 Switching Protocols")
  else
    let has_correct_accept =
      List.any
        lines
        ~fn:(fun line ->
          let trimmed = String.trim line in
          let lower = String.lowercase_ascii trimmed in
          String.starts_with ~prefix:"sec-websocket-accept:" lower && String.contains trimmed ":" && let parts =
            String.split ~by:":" trimmed
          in
          match parts with
          | [ _; value ] -> String.trim value = expected_accept
          | _ -> false)
    in
    if has_correct_accept then
      Ok ()
    else
      Error (Error.HandshakeFailed "Invalid Sec-WebSocket-Accept header")

let connect = fun uri ->
  let host =
    Net.Uri.host uri
    |> Option.unwrap_or ~default:"localhost"
  in
  match connect_transport uri host with
  | Error error -> Error error
  | Ok (transport, reader, writer) ->
      let conn = {
        transport;
        uri;
        reader;
        writer;
        buffer = Buffer.create ~size:4_096;
        closed = false;
      }
      in
      let key = generate_websocket_key () in
      let expected_accept = compute_accept_key key in
      let handshake =
        "GET "
        ^ Net.Uri.path_and_query uri
        ^ " HTTP/1.1\r\n"
        ^ "Host: "
        ^ host_header uri host
        ^ "\r\n"
        ^ "Upgrade: websocket\r\n"
        ^ "Connection: Upgrade\r\n"
        ^ "Sec-WebSocket-Key: "
        ^ key
        ^ "\r\n"
        ^ "Sec-WebSocket-Version: 13\r\n"
        ^ "\r\n"
      in
      match write_all conn handshake with
      | Error error ->
          close_transport transport;
          Error error
      | Ok () -> (
          match read_handshake_response conn with
          | Error error ->
              close_transport transport;
              Error error
          | Ok response -> (
              match validate_handshake expected_accept response with
              | Ok () -> Ok conn
              | Error error ->
                  close_transport transport;
                  Error error
            )
        )

let send_frame = fun conn frame ->
  if conn.closed then
    Error Error.Closed
  else
    let frame = Http.Ws.Frame.{ frame with masked = true } in
    match Http.Ws.Serializer.serialize frame with
    | Ok serialized -> write_all conn serialized
    | Error error -> Error (Error.WebSocketSerializeError error)

let send_text = fun conn text ->
  let frame = Http.Ws.Frame.text text in
  send_frame conn frame

let send_binary = fun conn data ->
  let frame = Http.Ws.Frame.binary data in
  send_frame conn frame

let send_ping = fun conn ?(payload = "") () ->
  let frame = Http.Ws.Frame.ping ~payload () in
  send_frame conn frame

let send_pong = fun conn ?(payload = "") () ->
  let frame = Http.Ws.Frame.pong ~payload () in
  send_frame conn frame

let send_close = fun conn ?(code = 1_000) ?(reason = "") () ->
  let payload =
    let code_bytes = IO.Bytes.create ~size:2 in
    IO.Bytes.set_unchecked code_bytes ~at:0 ~char:(Char.from_int_unchecked ((code lsr 8) land 0xff));
    IO.Bytes.set_unchecked code_bytes ~at:1 ~char:(Char.from_int_unchecked (code land 0xff));
    IO.Bytes.to_string code_bytes ^ reason
  in
  let frame = Http.Ws.Frame.close ~payload () in
  let result = send_frame conn frame in
  conn.closed <- true;
  result

let receive = fun conn ->
  if conn.closed then
    Error Error.Closed
  else
    let rec try_parse () =
      let data = Buffer.contents conn.buffer in
      match Http.Ws.Parser.parse ~role:Http.Ws.Parser.Client data with
      | Http.Ws.Parser.Done { value = frame; remaining } -> (
          Buffer.clear conn.buffer;
          Buffer.add_string conn.buffer remaining;
          match Http.Ws.Frame.(frame.opcode) with
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
                  (Char.code (String.get_unchecked frame.payload ~at:0) lsl 8)
                  lor Char.code (String.get_unchecked frame.payload ~at:1)
                in
                let reason =
                  if String.length frame.payload > 2 then
                    String.sub frame.payload ~offset:2 ~len:(String.length frame.payload - 2)
                  else
                    ""
                in
                Ok (Close (Some code, reason))
              else
                Ok (Close (None, ""))
          | Http.Ws.Frame.Continuation -> Error Error.InvalidFrame
        )
      | Http.Ws.Parser.Need_more -> (
          let chunk = IO.Buffer.create ~size:4_096 in
          match IO.read conn.reader ~into:chunk with
          | Error error -> Error (Error.from_io_error error)
          | Ok 0 -> Error Error.Eof
          | Ok _ ->
              let readable = IO.Buffer.readable chunk in
              let _ =
                Buffer.append_slice conn.buffer readable
                |> Result.expect ~msg:"failed to append websocket frame bytes"
              in
              try_parse ()
        )
      | Http.Ws.Parser.Error error -> Error (Error.WebSocketParseError error)
    in
    try_parse ()

let close = fun conn ->
  (
    if not conn.closed then
      let _ = send_close conn () in
      ()
  );
  close_transport conn.transport
