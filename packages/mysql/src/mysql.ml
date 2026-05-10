open Std

module Error = Protocol.Error
module Buffer = Std.StringBuilder

module Internal = struct
  module Protocol = Protocol
end

module Config = struct
  type ssl_mode =
    | Disable
    | Prefer
    | Require

  type t = {
    host: string;
    port: int;
    database: string option;
    user: string;
    password: string;
    ssl_mode: ssl_mode;
    collation_id: int;
    connect_timeout: Time.Duration.t;
    keepalives_idle: Time.Duration.t option;
  }

  type parse_error =
    | InvalidUserinfoFormat
    | InvalidAuthorityFormat
    | MissingUserCredentials
    | InvalidPortNumber of string
    | InvalidConnectionStringFormat
    | InvalidUri

  let parse_error_to_string = fun error ->
    match error with
    | InvalidUserinfoFormat -> "invalid userinfo format in URI"
    | InvalidAuthorityFormat -> "invalid authority format in URI"
    | MissingUserCredentials -> "missing user credentials in URI"
    | InvalidPortNumber value -> "invalid port number: " ^ value
    | InvalidConnectionStringFormat -> "invalid connection string format (use 'mysql://user:pass@host:port/db' or 'host:port:database:user:password')"
    | InvalidUri -> "failed to parse connection string"

  let default = fun () ->
    {
      host = "localhost";
      port = 3_306;
      database = None;
      user = "root";
      password = "";
      ssl_mode = Prefer;
      collation_id = 45;
      connect_timeout = Time.Duration.from_secs 10;
      keepalives_idle = None;
    }

  let parse_port = fun value ->
    match Int.parse value with
    | Some port when port > 0 && port <= 65_535 -> Ok port
    | _ -> Error (InvalidPortNumber value)

  let split_userinfo = fun value ->
    match String.index_of value ~char:':' with
    | None -> Ok (Net.Uri.percent_decode value, "")
    | Some index ->
        let user = String.sub value ~offset:0 ~len:index in
        let password =
          String.sub value ~offset:(index + 1) ~len:(String.length value - index - 1)
        in
        Ok (Net.Uri.percent_decode user, Net.Uri.percent_decode password)

  let split_authority = fun authority ->
    match String.last_index authority '@' with
    | None -> Error MissingUserCredentials
    | Some index when index = 0 || index + 1 >= String.length authority ->
        Error MissingUserCredentials
    | Some index ->
        let userinfo = String.sub authority ~offset:0 ~len:index in
        let hostinfo =
          String.sub authority ~offset:(index + 1) ~len:(String.length authority - index - 1)
        in
        Ok (userinfo, hostinfo)

  let split_host_port = fun value ->
    if String.length value = 0 then
      Error InvalidAuthorityFormat
    else if String.get_unchecked value ~at:0 = '[' then
      match String.index_of value ~char:']' with
      | None -> Error InvalidAuthorityFormat
      | Some close_index ->
          let host = String.sub value ~offset:1 ~len:(close_index - 1) in
          if String.length host = 0 then
            Error InvalidAuthorityFormat
          else if close_index + 1 = String.length value then
            Ok (host, 3_306)
          else if String.get_unchecked value ~at:(close_index + 1) != ':' then
            Error InvalidAuthorityFormat
          else
            let port_text =
              String.sub
                value
                ~offset:(close_index + 2)
                ~len:(String.length value - close_index - 2)
            in
            Result.map (parse_port port_text) ~fn:(fun port -> (host, port))
    else
      match String.last_index value ':' with
      | None -> Ok (value, 3_306)
      | Some index ->
          let host = String.sub value ~offset:0 ~len:index in
          let port_text =
            String.sub value ~offset:(index + 1) ~len:(String.length value - index - 1)
          in
          if String.length host = 0 then
            Error InvalidAuthorityFormat
          else if String.contains host ":" then
            Ok (value, 3_306)
          else
            Result.map (parse_port port_text) ~fn:(fun port -> (host, port))

  let make = fun ~host ~port ~database ~user ~password ->
    if String.length user = 0 then
      Error MissingUserCredentials
    else
      Ok {
        host;
        port;
        database;
        user;
        password;
        ssl_mode = Prefer;
        collation_id = 45;
        connect_timeout = Time.Duration.from_secs 10;
        keepalives_idle = None;
      }

  let from_string = fun str ->
    match Net.Uri.from_string str with
    | Ok uri when Net.Uri.scheme uri = Some "mysql" -> (
        let path = Net.Uri.path uri in
        let database =
          if String.length path <= 1 then
            None
          else if String.get_unchecked path ~at:0 = '/' then
            Some (
              String.sub path ~offset:1 ~len:(String.length path - 1)
              |> Net.Uri.percent_decode
            )
          else
            None
        in
        match Net.Uri.authority uri with
        | Some auth_str -> (
            match split_authority auth_str with
            | Error _ as error -> error
            | Ok (userinfo, hostinfo) -> (
                match split_userinfo userinfo with
                | Error _ as error -> error
                | Ok (user, password) -> (
                    match split_host_port hostinfo with
                    | Error _ as error -> error
                    | Ok (host, port) ->
                        make ~host:(Net.Uri.percent_decode host) ~port ~database ~user ~password
                  )
              )
          )
        | None -> Error MissingUserCredentials
      )
    | Ok _ -> (
        match String.split_on_char ':' str with
        | [ host; port_str; database; user; password ] -> (
            match parse_port port_str with
            | Ok port -> make ~host ~port ~database:(Some database) ~user ~password
            | Error _ as error -> error
          )
        | _ -> Error InvalidConnectionStringFormat
      )
    | Error _ -> Error InvalidUri
end

module Driver = struct
  module Queue = Collections.Queue
  module Row = Sqlx_driver.Row
  module Ser = Serde.Ser
  module Value = Sqlx_driver.Value

  type config = Config.t

  type error =
    | TransportError of string
    | ProtocolError of Protocol.parse_error
    | ServerError of Protocol.Error.t
    | ConnectionClosed
    | AuthenticationNotSupported of string
    | TlsNotSupported of string
    | TlsError of string
    | UnexpectedMessage of string
    | UnsupportedOperation of string

  type transport = {
    tcp: Net.TcpStream.t;
    mutable reader: IO.Reader.t;
    mutable writer: IO.Writer.t;
    mutable tls: Net.TcpStream.t Net.TlsStream.t option;
  }

  type connection = {
    id: string;
    transport: transport;
    config: config;
    mutable closed: bool;
    mutable server_status: Protocol.ServerStatus.t;
    mutable last_insert_id: int64;
  }

  type statement = {
    sql: string;
    conn: connection;
  }

  type result_set = {
    rows: Row.t Queue.t;
    mutable rows_affected: int;
  }

  type timed_tcp = {
    stream: Net.TcpStream.t;
    timeout: Time.Duration.t;
  }

  let name = "MySQL"

  let max_accumulated_packet_payload_length = 64 * 1_024 * 1_024

  let max_result_columns = 4_096

  let max_buffered_result_rows = 100_000

  let io_error_of_tcp_error = fun error ->
    match error with
    | Kernel.Net.TcpStream.InvalidSlice _ -> IO.Invalid_argument
    | Kernel.Net.TcpStream.InvalidSocketAddr _ -> IO.Invalid_argument
    | Kernel.Net.TcpStream.InvalidConnectState _ -> IO.Invalid_argument
    | Kernel.Net.TcpStream.WouldBlock -> IO.Operation_would_block
    | Kernel.Net.TcpStream.ConnectionRefused -> IO.Connection_refused
    | Kernel.Net.TcpStream.ConnectionReset -> IO.Connection_reset_by_peer
    | Kernel.Net.TcpStream.TimedOut -> IO.Connection_timed_out
    | Kernel.Net.TcpStream.BrokenPipe -> IO.Broken_pipe
    | Kernel.Net.TcpStream.NotConnected -> IO.Transport_endpoint_not_connected
    | Kernel.Net.TcpStream.ConnectionAborted -> IO.Software_caused_connection_abort
    | Kernel.Net.TcpStream.NetworkUnreachable -> IO.Network_is_unreachable
    | Kernel.Net.TcpStream.System error -> IO.from_system_error error

  let close_kernel_tcp = fun stream -> ignore (Kernel.Net.TcpStream.close stream)

  let connect_tcp_with_timeout = fun addr timeout ->
    let timeout = Time.Duration.to_secs_float timeout in
    let close_and_error stream error =
      close_kernel_tcp stream;
      Error error
    in
    let rec finish_connect stream =
      let source = Kernel.Net.TcpStream.to_source stream in
      match Kernel.Net.TcpStream.finish_connect stream with
      | Ok () -> Ok stream
      | Error Kernel.Net.TcpStream.WouldBlock -> (
          try Runtime.syscall
            ~timeout
            ~name:"Mysql.TcpStream.connect"
            ~interest:Kernel.Async.Interest.writable
            ~source
            (fun () -> finish_connect stream) with
          | Syscall_timeout ->
              close_and_error stream (Net.TcpStream.System_error IO.Connection_timed_out)
        )
      | Error Kernel.Net.TcpStream.ConnectionRefused ->
          close_and_error stream Net.TcpStream.Connection_refused
      | Error error ->
          close_and_error stream (Net.TcpStream.System_error (io_error_of_tcp_error error))
    in
    match Kernel.Net.TcpStream.connect addr with
    | Ok (Kernel.Net.TcpStream.Connected stream) -> Ok stream
    | Ok (Kernel.Net.TcpStream.InProgress stream) -> finish_connect stream
    | Error Kernel.Net.TcpStream.ConnectionRefused -> Error Net.TcpStream.Connection_refused
    | Error error -> Error (Net.TcpStream.System_error (io_error_of_tcp_error error))

  let wait_tcp = fun ~name ~interest ~source ~timeout fn ->
    try Runtime.syscall ~timeout:(Time.Duration.to_secs_float timeout) ~name ~interest ~source fn with
    | Syscall_timeout -> Error IO.Connection_timed_out

  let timed_reader = fun stream timeout ->
    let module Read = struct
      type t = timed_tcp

      let commit_into = fun into count ->
        match IO.Buffer.commit into count with
        | Ok () -> Ok count
        | Error error ->
            Kernel.SystemError.panic ("Mysql.timed_reader.commit: " ^ Kernel.IO.Error.message error)

      let writable = fun into ->
        if IO.Buffer.writable_bytes into = 0 then (
          match IO.Buffer.ensure_free into 4_096 with
          | Ok () -> IO.Buffer.writable into
          | Error error ->
              Kernel.SystemError.panic
                ("Mysql.timed_reader.ensure_free: " ^ Kernel.IO.Error.message error)
        ) else
          IO.Buffer.writable into

      let rec read = fun t ~into ->
        let writable = writable into in
        let bufs = IO.IoVec.from_slices [|writable|] in
        match read_vectored t ~into:bufs with
        | Ok count -> commit_into into count
        | Error _ as error -> error

      and read_vectored = fun t ~into ->
        let source = Kernel.Net.TcpStream.to_source t.stream in
        let rec loop () =
          match Kernel.Net.TcpStream.read_vectored t.stream into with
          | Ok n -> Ok n
          | Error Kernel.Net.TcpStream.WouldBlock ->
              wait_tcp
                ~name:"Mysql.TcpStream.read_vectored"
                ~interest:Kernel.Async.Interest.readable
                ~source
                ~timeout:t.timeout
                loop
          | Error error -> Error (io_error_of_tcp_error error)
        in
        loop ()

      let is_read_vectored = fun _ -> true
    end in
    IO.Reader.from_source (module Read) { stream; timeout }

  let timed_writer = fun stream timeout ->
    let module Write = struct
      type t = timed_tcp

      let write = fun t ~from ->
        let source = Kernel.Net.TcpStream.to_source t.stream in
        let rec loop () =
          match Kernel.Net.TcpStream.write_vectored t.stream (IO.Buffer.to_iovec from) with
          | Ok n -> Ok n
          | Error Kernel.Net.TcpStream.WouldBlock ->
              wait_tcp
                ~name:"Mysql.TcpStream.write"
                ~interest:Kernel.Async.Interest.writable
                ~source
                ~timeout:t.timeout
                loop
          | Error error -> Error (io_error_of_tcp_error error)
        in
        loop ()

      let write_vectored = fun t ~from ->
        let source = Kernel.Net.TcpStream.to_source t.stream in
        let rec loop () =
          match Kernel.Net.TcpStream.write_vectored t.stream from with
          | Ok n -> Ok n
          | Error Kernel.Net.TcpStream.WouldBlock ->
              wait_tcp
                ~name:"Mysql.TcpStream.write_vectored"
                ~interest:Kernel.Async.Interest.writable
                ~source
                ~timeout:t.timeout
                loop
          | Error error -> Error (io_error_of_tcp_error error)
        in
        loop ()

      let flush = fun _ -> Ok ()
    end in
    IO.Writer.from_sink (module Write) { stream; timeout }

  let tls_error_to_string = fun error ->
    match error with
    | Net.TlsStream.Closed -> "TLS stream closed"
    | Net.TlsStream.Handshake_failed message -> "TLS handshake failed: " ^ message
    | Net.TlsStream.System_error error -> IO.Error.message error
    | Net.TlsStream.Network_read_failed error ->
        "TLS network read failed: " ^ IO.Error.message error
    | Net.TlsStream.Network_write_failed error ->
        "TLS network write failed: " ^ IO.Error.message error
    | Net.TlsStream.Tls_not_available -> "TLS is not available"
    | Net.TlsStream.Unsupported_vectored_operation -> "TLS vectored operation is not supported"

  let error_to_string = fun error ->
    match error with
    | TransportError message -> "Transport error: " ^ message
    | ProtocolError error -> Protocol.parse_error_to_string error
    | ServerError error -> Protocol.Error.to_string error
    | ConnectionClosed -> "Connection is closed"
    | AuthenticationNotSupported plugin -> "Authentication method not supported: " ^ plugin
    | TlsNotSupported message -> "MySQL TLS is not supported: " ^ message
    | TlsError message -> "MySQL TLS error: " ^ message
    | UnexpectedMessage message -> "Unexpected MySQL message: " ^ message
    | UnsupportedOperation message -> "Unsupported MySQL operation: " ^ message

  type error_report = {
    kind: string;
    message: string option;
    code: int option;
    sql_state: string option;
    plugin: string option;
  }

  let error_report_serializer =
    Ser.record
      (
        Ser.fields
          [
            Ser.field "type" Ser.string (fun (report: error_report) -> report.kind);
            Ser.field
              "message"
              (Ser.option Ser.string)
              (fun (report: error_report) -> report.message);
            Ser.field "code" (Ser.option Ser.int) (fun (report: error_report) -> report.code);
            Ser.field
              "sql_state"
              (Ser.option Ser.string)
              (fun (report: error_report) -> report.sql_state);
            Ser.field "plugin" (Ser.option Ser.string) (fun (report: error_report) -> report.plugin);
          ]
      )

  let error_to_report = fun error ->
    let report ?message ?code ?sql_state ?plugin kind = {
      kind;
      message;
      code;
      sql_state;
      plugin;
    }
    in
    match error with
    | TransportError message -> report ~message "transport_error"
    | ProtocolError error -> report ~message:(Protocol.parse_error_to_string error) "protocol_error"
    | ServerError error ->
        report ~message:error.message ~code:error.code ?sql_state:error.sql_state "mysql_error"
    | ConnectionClosed -> report ~message:"Connection is closed" "connection_closed"
    | AuthenticationNotSupported plugin -> report ~plugin "authentication_not_supported"
    | TlsNotSupported message -> report ~message "tls_not_supported"
    | TlsError message -> report ~message "tls_error"
    | UnexpectedMessage message -> report ~message "unexpected_message"
    | UnsupportedOperation message -> report ~message "unsupported_operation"

  let error_serializer = Ser.contramap error_to_report error_report_serializer

  let error_to_json_string = fun error -> Serde_json.to_string error_serializer error

  let error_report_to_json = fun report ->
    let fields = [ ("type", Data.Json.string report.kind) ] in
    let fields =
      match report.message with
      | Some message -> fields @ [ ("message", Data.Json.string message) ]
      | None -> fields
    in
    let fields =
      match report.code with
      | Some code -> fields @ [ ("code", Data.Json.int code) ]
      | None -> fields
    in
    let fields =
      match report.sql_state with
      | Some sql_state -> fields @ [ ("sql_state", Data.Json.string sql_state) ]
      | None -> fields
    in
    let fields =
      match report.plugin with
      | Some plugin -> fields @ [ ("plugin", Data.Json.string plugin) ]
      | None -> fields
    in
    Data.Json.obj fields

  let error_to_json = fun error ->
    error_to_report error
    |> error_report_to_json

  let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

  let read_exact = fun transport len ->
    let buffer = IO.Buffer.create ~size:len in
    match IO.Reader.read_exact transport.reader ~into:buffer ~len with
    | Ok () -> Ok (IO.Buffer.to_string buffer)
    | Error error -> Error (TransportError (IO.Error.message error))

  let write_all = fun transport payload ->
    match IO.Writer.write_all transport.writer ~from:(IO.Buffer.from_string payload) with
    | Error error -> Error (TransportError (IO.Error.message error))
    | Ok () -> (
        match IO.Writer.flush transport.writer with
        | Ok () -> Ok ()
        | Error error -> Error (TransportError (IO.Error.message error))
      )

  let packet_length = fun header ->
    byte_at header 0 lor (byte_at header 1 lsl 8) lor (byte_at header 2 lsl 16)

  let read_packet = fun transport ->
    let payload = Buffer.create ~size:128 in
    let rec loop expected_sequence =
      match read_exact transport 4 with
      | Error _ as error -> error
      | Ok header ->
          let length = packet_length header in
          let sequence = byte_at header 3 in
          if length > Protocol.Packet.max_payload_length then
            Error (ProtocolError (Protocol.InvalidPacketLength length))
          else if expected_sequence >= 0 && sequence != expected_sequence land 0xff then
            Error (ProtocolError (Protocol.InvalidPacketSequence {
              expected = expected_sequence land 0xff;
              actual = sequence;
            }))
          else if Buffer.length payload + length > max_accumulated_packet_payload_length then
            Error (ProtocolError (Protocol.InvalidPayload ("packet payload exceeded "
            ^ Int.to_string max_accumulated_packet_payload_length
            ^ " bytes")))
          else
            match read_exact transport length with
            | Error _ as error -> error
            | Ok chunk ->
                Buffer.add_string payload chunk;
                if length = Protocol.Packet.max_payload_length then
                  loop ((sequence + 1) land 0xff)
                else
                  Ok Protocol.Packet.{ sequence; payload = Buffer.contents payload }
    in
    loop (-1)

  let write_packet = fun transport ~sequence ~payload ->
    let frames = Protocol.Writer.packet ~sequence ~payload in
    let rec loop frames =
      match frames with
      | [] -> Ok ()
      | frame :: rest -> (
          match write_all transport frame with
          | Error _ as error -> error
          | Ok () -> loop rest
        )
    in
    loop frames

  let is_error_packet = fun payload ->
    if String.length payload = 0 then
      false
    else
      byte_at payload 0 = 0xff

  let is_result_terminator = fun payload ->
    let length = String.length payload in
    if length = 0 || length > 5 then
      false
    else
      byte_at payload 0 = 0xfe

  let update_status_from_ok = fun conn ok ->
    conn.server_status <- Protocol.(ok.status);
    conn.last_insert_id <- Protocol.(ok.last_insert_id)

  let update_status_from_eof = fun conn payload ->
    if String.length payload >= 5 then
      let flags = byte_at payload 3 lor (byte_at payload 4 lsl 8) in
      conn.server_status <- Protocol.ServerStatus.from_int flags

  let result_empty = fun ?(rows_affected = 0) () -> { rows = Queue.create (); rows_affected }

  let parse_lenenc_int = fun payload ->
    if String.length payload = 0 then
      Error (Protocol.Truncated "length-encoded integer")
    else
      let first = byte_at payload 0 in
      if first < 0xfb then
        Ok (first, 1)
      else
        match first with
        | 0xfc when String.length payload >= 3 ->
            Ok (byte_at payload 1 lor (byte_at payload 2 lsl 8), 3)
        | 0xfd when String.length payload >= 4 ->
            Ok (byte_at payload 1 lor (byte_at payload 2 lsl 8) lor (byte_at payload 3 lsl 16), 4)
        | 0xfe when String.length payload >= 9 ->
            let value = ref 0L in
            for index = 0 to 7 do
              value := Int64.logor
                !value
                (Int64.shift_left (Int64.from_int (byte_at payload (index + 1))) (index * 8))
            done;
            if Int64.compare !value (Int64.from_int Int.max_int) = Order.GT then
              Error (Protocol.InvalidLengthEncodedInteger "column count")
            else
              Ok (Int64.to_int !value, 9)
        | _ -> Error (Protocol.InvalidLengthEncodedInteger "column count")

  let read_column_definitions = fun conn count ->
    let rec loop remaining acc =
      if remaining = 0 then
        Ok (List.rev acc)
      else
        match read_packet conn.transport with
        | Error _ as error -> error
        | Ok packet ->
            if is_error_packet packet.payload then
              match Protocol.Reader.parse_error_packet packet.payload with
              | Ok error -> Error (ServerError error)
              | Error error -> Error (ProtocolError error)
            else
              match Protocol.Reader.parse_column_definition packet.payload with
              | Error error -> Error (ProtocolError error)
              | Ok column -> loop (remaining - 1) (column :: acc)
    in
    loop count []

  let drain_metadata_terminator = fun conn count ->
    if count = 0 then
      Ok ()
    else
      match read_packet conn.transport with
      | Error _ as error -> error
      | Ok packet when is_result_terminator packet.payload ->
          update_status_from_eof conn packet.payload;
          Ok ()
      | Ok packet when is_error_packet packet.payload -> (
          match Protocol.Reader.parse_error_packet packet.payload with
          | Ok error -> Error (ServerError error)
          | Error error -> Error (ProtocolError error)
        )
      | Ok _ -> Error (UnexpectedMessage "expected column metadata terminator")

  let read_result_set = fun conn ~binary first_payload ->
    match parse_lenenc_int first_payload with
    | Error error -> Error (ProtocolError error)
    | Ok (column_count, _) when column_count > max_result_columns ->
        Error (UnsupportedOperation ("result set has too many columns: "
        ^ Int.to_string column_count))
    | Ok (column_count, _) -> (
        match read_column_definitions conn column_count with
        | Error _ as error -> error
        | Ok columns -> (
            match drain_metadata_terminator conn column_count with
            | Error _ as error -> error
            | Ok () ->
                let result = result_empty () in
                let rec read_rows count =
                  match read_packet conn.transport with
                  | Error _ as error -> error
                  | Ok packet ->
                      if is_result_terminator packet.payload then (
                        update_status_from_eof conn packet.payload;
                        Ok result
                      ) else if is_error_packet packet.payload then
                        match Protocol.Reader.parse_error_packet packet.payload with
                        | Ok error -> Error (ServerError error)
                        | Error error -> Error (ProtocolError error)
                      else if count >= max_buffered_result_rows then
                        Error (UnsupportedOperation ("result set exceeded buffered row limit: "
                        ^ Int.to_string max_buffered_result_rows))
                      else
                        let parsed =
                          if binary then
                            Protocol.Reader.parse_binary_row columns packet.payload
                          else
                            Protocol.Reader.parse_text_row columns packet.payload
                        in
                        match parsed with
                        | Error error -> Error (ProtocolError error)
                        | Ok row ->
                            Queue.push result.rows ~value:row;
                            read_rows (count + 1)
                in
                read_rows 0
          )
      )

  let read_command_response = fun conn ~binary ->
    match read_packet conn.transport with
    | Error _ as error -> error
    | Ok packet ->
        if String.length packet.payload = 0 then
          Error (UnexpectedMessage "empty response packet")
        else
          let marker = byte_at packet.payload 0 in
          match marker with
          | 0x00 -> (
              match Protocol.Reader.parse_ok_packet packet.payload with
              | Error error -> Error (ProtocolError error)
              | Ok ok ->
                  update_status_from_ok conn ok;
                  Ok (result_empty ~rows_affected:(Int64.to_int Protocol.(ok.affected_rows)) ())
            )
          | 0xfe when is_result_terminator packet.payload -> (
              match Protocol.Reader.parse_ok_packet packet.payload with
              | Error error -> Error (ProtocolError error)
              | Ok ok ->
                  update_status_from_ok conn ok;
                  Ok (result_empty ~rows_affected:(Int64.to_int Protocol.(ok.affected_rows)) ())
            )
          | 0xff -> (
              match Protocol.Reader.parse_error_packet packet.payload with
              | Ok error -> Error (ServerError error)
              | Error error -> Error (ProtocolError error)
            )
          | 0xfb -> Error (UnsupportedOperation "LOCAL INFILE requests are not supported")
          | _ -> read_result_set conn ~binary packet.payload

  let send_command = fun conn payload ->
    if conn.closed then
      Error ConnectionClosed
    else
      write_packet conn.transport ~sequence:0 ~payload

  let execute_command = fun conn ~binary payload ->
    match send_command conn payload with
    | Error _ as error -> error
    | Ok () -> read_command_response conn ~binary

  let scramble = fun plugin ~password ~seed ->
    match plugin with
    | "mysql_native_password" -> Ok (Protocol.Auth.mysql_native_password ~password ~seed)
    | "caching_sha2_password" -> Ok (Protocol.Auth.caching_sha2_password ~password ~seed)
    | "" -> Ok ""
    | plugin -> Error (AuthenticationNotSupported plugin)

  let parse_auth_switch = fun payload ->
    if String.length payload = 0 then
      Error (UnexpectedMessage "expected auth switch request")
    else if byte_at payload 0 != 0xfe then
      Error (UnexpectedMessage "expected auth switch request")
    else
      let rec find_nul index =
        if index >= String.length payload then
          None
        else if byte_at payload index = 0 then
          Some index
        else
          find_nul (index + 1)
      in
      match find_nul 1 with
      | None -> Error (UnexpectedMessage "auth switch request missing plugin name")
      | Some nul ->
          let plugin = String.sub payload ~offset:1 ~len:(nul - 1) in
          let seed = String.sub payload ~offset:(nul + 1) ~len:(String.length payload - nul - 1) in
          Ok (plugin, seed)

  let authenticate_response = fun transport cfg handshake ~sequence ~plugin ~seed ->
    match scramble plugin ~password:cfg.Config.password ~seed with
    | Error _ as error -> error
    | Ok auth_response ->
        let payload =
          Protocol.Writer.handshake_response
            ~capability_flags:(Protocol.Capability.default_client
              ~database:(Option.is_some cfg.database)
              ~ssl:(Option.is_some transport.tls)
              ()
            land handshake.Protocol.capability_flags)
            ~max_packet_size:(16 * 1_024 * 1_024)
            ~character_set:cfg.collation_id
            ~user:cfg.user
            ~database:cfg.database
            ~auth_response
            ~auth_plugin:plugin
        in
        write_packet transport ~sequence ~payload

  let rec read_auth_result = fun transport cfg packet ->
    if String.length packet.Protocol.Packet.payload = 0 then
      Error (UnexpectedMessage "empty authentication response")
    else
      let payload = packet.Protocol.Packet.payload in
      match byte_at payload 0 with
      | 0x00 -> (
          match Protocol.Reader.parse_ok_packet payload with
          | Ok _ -> Ok ()
          | Error error -> Error (ProtocolError error)
        )
      | 0xff -> (
          match Protocol.Reader.parse_error_packet payload with
          | Ok error -> Error (ServerError error)
          | Error error -> Error (ProtocolError error)
        )
      | 0xfe -> (
          match parse_auth_switch payload with
          | Error _ as error -> error
          | Ok (plugin, seed) -> (
              match scramble plugin ~password:cfg.Config.password ~seed with
              | Error _ as error -> error
              | Ok response -> (
                  match write_packet
                    transport
                    ~sequence:((packet.sequence + 1) land 0xff)
                    ~payload:response with
                  | Error _ as error -> error
                  | Ok () -> (
                      match read_packet transport with
                      | Error _ as error -> error
                      | Ok packet -> read_auth_result transport cfg packet
                    )
                )
            )
        )
      | 0x01 ->
          if String.length payload < 2 then
            Error (UnexpectedMessage "truncated auth more data packet")
          else
            let status = byte_at payload 1 in
            if status = 0x03 then
              match read_packet transport with
              | Error _ as error -> error
              | Ok packet -> read_auth_result transport cfg packet
            else if status = 0x04 then
              match transport.tls with
              | Some _ -> (
                  match write_packet
                    transport
                    ~sequence:((packet.sequence + 1) land 0xff)
                    ~payload:(cfg.password ^ "\x00") with
                  | Error _ as error -> error
                  | Ok () -> (
                      match read_packet transport with
                      | Error _ as error -> error
                      | Ok packet -> read_auth_result transport cfg packet
                    )
                )
              | None ->
                  Error (UnsupportedOperation "caching_sha2_password full authentication requires TLS; RSA key exchange is not implemented")
            else
              Error (UnexpectedMessage ("unknown auth more data status: " ^ Int.to_string status))
      | marker ->
          Error (UnexpectedMessage ("unexpected authentication marker: " ^ Int.to_string marker))

  let should_use_tls = fun cfg handshake ->
    let server_supports_tls =
      Protocol.Capability.has handshake.Protocol.capability_flags Protocol.Capability.ssl
    in
    match (cfg.Config.ssl_mode, server_supports_tls) with
    | (Config.Disable, _) -> Ok false
    | (Config.Require, false) -> Error (TlsNotSupported "server did not advertise CLIENT_SSL")
    | (Config.Require, true)
    | (Config.Prefer, true) -> Ok true
    | (Config.Prefer, false) -> Ok false

  let upgrade_tls = fun transport cfg handshake ->
    match should_use_tls cfg handshake with
    | Error _ as error -> error
    | Ok false -> Ok 1
    | Ok true ->
        let capability_flags =
          Protocol.Capability.default_client
            ~database:(Option.is_some cfg.Config.database)
            ~ssl:true
            ()
          land handshake.Protocol.capability_flags
        in
        let ssl_request =
          Protocol.Writer.ssl_request
            ~capability_flags
            ~max_packet_size:(16 * 1_024 * 1_024)
            ~character_set:cfg.collation_id
        in
        match write_packet transport ~sequence:1 ~payload:ssl_request with
        | Error _ as error -> error
        | Ok () -> (
            match Net.TlsStream.from_client_io
              ~reader:transport.reader
              ~writer:transport.writer
              ~hostname:cfg.host
              () with
            | Error error -> Error (TlsError (tls_error_to_string error))
            | Ok tls ->
                transport.tls <- Some tls;
                transport.reader <- Net.TlsStream.to_reader tls;
                transport.writer <- Net.TlsStream.to_writer tls;
                Ok 2
          )

  let perform_handshake = fun transport cfg ->
    match read_packet transport with
    | Error _ as error -> error
    | Ok packet -> (
        match Protocol.Reader.parse_handshake packet.payload with
        | Error error -> Error (ProtocolError error)
        | Ok handshake ->
            if
              not
                (Protocol.Capability.has handshake.capability_flags Protocol.Capability.protocol_41)
            then
              Error (UnsupportedOperation "server does not support the MySQL 4.1 protocol")
            else if
              not
                (Protocol.Capability.has
                  handshake.capability_flags
                  Protocol.Capability.secure_connection)
            then
              Error (UnsupportedOperation "server does not support secure connection auth")
            else if
              not
                (Protocol.Capability.has
                  handshake.capability_flags
                  Protocol.Capability.plugin_auth_lenenc_client_data)
            then
              Error (UnsupportedOperation "server does not support length-encoded authentication responses")
            else
              match upgrade_tls transport cfg handshake with
              | Error _ as error -> error
              | Ok response_sequence ->
                  let plugin =
                    handshake.auth_plugin_name
                    |> Option.unwrap_or ~default:"mysql_native_password"
                  in
                  match authenticate_response
                    transport
                    cfg
                    handshake
                    ~sequence:response_sequence
                    ~plugin
                    ~seed:handshake.auth_plugin_data with
                  | Error _ as error -> error
                  | Ok () -> (
                      match read_packet transport with
                      | Error _ as error -> error
                      | Ok packet -> read_auth_result transport cfg packet
                    )
      )

  let execute_simple_command = fun conn sql ->
    match execute_command conn ~binary:false (Protocol.Writer.com_query sql) with
    | Error _ as error -> error
    | Ok _ -> Ok ()

  let connect = fun cfg ->
    match cfg.Config.keepalives_idle with
    | Some _ ->
        Error (UnsupportedOperation "TCP keepalive configuration is not supported by this runtime yet")
    | None -> (
        match Net.Addr.from_host_and_port ~host:cfg.Config.host ~port:cfg.port with
        | Error _ -> Error (TransportError "failed to resolve address")
        | Ok addr -> (
            match connect_tcp_with_timeout addr cfg.connect_timeout with
            | Error Net.TcpStream.Connection_refused -> Error (TransportError "connection refused")
            | Error Net.TcpStream.Closed -> Error (TransportError "connection closed")
            | Error (Net.TcpStream.System_error error) ->
                Error (TransportError (IO.Error.message error))
            | Ok tcp ->
                let transport = {
                  tcp;
                  reader = timed_reader tcp cfg.connect_timeout;
                  writer = timed_writer tcp cfg.connect_timeout;
                  tls = None;
                }
                in
                match perform_handshake transport cfg with
                | Error error ->
                    Net.TcpStream.close tcp;
                    Error error
                | Ok () ->
                    let conn = {
                      id = "mysql_" ^ UUID.to_string (UUID.v7_monotonic ());
                      transport;
                      config = cfg;
                      closed = false;
                      server_status = Protocol.ServerStatus.from_int 0x0002;
                      last_insert_id = 0L;
                    }
                    in
                    match execute_simple_command conn "SET time_zone = '+00:00'" with
                    | Error error ->
                        Net.TcpStream.close tcp;
                        Error error
                    | Ok () -> Ok conn
          )
      )

  let close = fun conn ->
    if not conn.closed then (
      conn.closed <- true;
      ignore (write_packet conn.transport ~sequence:0 ~payload:(Protocol.Writer.com_quit ()));
      (
        match conn.transport.tls with
        | Some tls -> Net.TlsStream.close tls
        | None -> ()
      );
      Net.TcpStream.close conn.transport.tcp
    )

  let ping = fun conn -> not conn.closed

  let prepare = fun conn sql ->
    if conn.closed then
      Error ConnectionClosed
    else
      Ok { sql; conn }

  let drain_prepare_metadata = fun conn prepared ->
    match read_column_definitions conn Protocol.(prepared.num_params) with
    | Error _ as error -> error
    | Ok _ -> (
        match drain_metadata_terminator conn Protocol.(prepared.num_params) with
        | Error _ as error -> error
        | Ok () -> (
            match read_column_definitions conn Protocol.(prepared.num_columns) with
            | Error _ as error -> error
            | Ok _ -> drain_metadata_terminator conn Protocol.(prepared.num_columns)
          )
      )

  let prepare_server_statement = fun conn sql ->
    match send_command conn (Protocol.Writer.com_stmt_prepare sql) with
    | Error _ as error -> error
    | Ok () -> (
        match read_packet conn.transport with
        | Error _ as error -> error
        | Ok packet ->
            if is_error_packet packet.payload then
              match Protocol.Reader.parse_error_packet packet.payload with
              | Ok error -> Error (ServerError error)
              | Error error -> Error (ProtocolError error)
            else
              match Protocol.Reader.parse_prepare_ok packet.payload with
              | Error error -> Error (ProtocolError error)
              | Ok prepared -> (
                  match drain_prepare_metadata conn prepared with
                  | Error _ as error -> error
                  | Ok () -> Ok prepared.statement_id
                )
      )

  let close_server_statement = fun conn statement_id ->
    write_packet
      conn.transport
      ~sequence:0
      ~payload:(Protocol.Writer.com_stmt_close statement_id)

  let execute = fun stmt params ->
    if stmt.conn.closed then
      Error ConnectionClosed
    else if List.is_empty params then
      execute_command stmt.conn ~binary:false (Protocol.Writer.com_query stmt.sql)
    else
      match prepare_server_statement stmt.conn stmt.sql with
      | Error _ as error -> error
      | Ok statement_id ->
          let payload = Protocol.Writer.com_stmt_execute ~statement_id ~params in
          let result =
            match send_command stmt.conn payload with
            | Error _ as error -> error
            | Ok () -> read_command_response stmt.conn ~binary:true
          in
          ignore (close_server_statement stmt.conn statement_id);
          result

  let fetch_row = fun result_set -> Queue.pop result_set.rows

  let rows_affected = fun result_set -> result_set.rows_affected

  let begin_transaction = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.server_status.Protocol.ServerStatus.in_transaction then
      Error (UnexpectedMessage "transaction already in progress")
    else
      execute_simple_command conn "START TRANSACTION"

  let commit = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if not conn.server_status.Protocol.ServerStatus.in_transaction then
      Error (UnexpectedMessage "no transaction in progress")
    else
      execute_simple_command conn "COMMIT"

  let rollback = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if not conn.server_status.Protocol.ServerStatus.in_transaction then
      Error (UnexpectedMessage "no transaction in progress")
    else
      execute_simple_command conn "ROLLBACK"

  let isolation_level_sql = fun level ->
    match level with
    | Sqlx_driver.Driver.ReadUncommitted -> "READ UNCOMMITTED"
    | Sqlx_driver.Driver.ReadCommitted -> "READ COMMITTED"
    | Sqlx_driver.Driver.RepeatableRead -> "REPEATABLE READ"
    | Sqlx_driver.Driver.Serializable -> "SERIALIZABLE"

  let set_isolation_level = fun conn level ->
    if conn.closed then
      Error ConnectionClosed
    else
      let sql =
        if conn.server_status.Protocol.ServerStatus.in_transaction then
          "SET TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
        else
          "SET SESSION TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
      in
      execute_simple_command conn sql
end
