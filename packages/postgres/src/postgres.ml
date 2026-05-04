open Std
open Std.IO
open Result.Syntax

module Error = Protocol.Error

module Internal = struct
  module Protocol = Protocol
end

module Config = struct
  type ssl_mode =
    | Disable
    | Require
    | Prefer

  type t = {
    host: string;
    port: int;
    database: string;
    user: string;
    password: string;
    ssl_mode: ssl_mode;
    application_name: string option;
    connect_timeout: Time.Duration.t;
    keepalives_idle: Time.Duration.t option;
  }

  let default = fun () ->
    {
      host = "localhost";
      port = 5_432;
      database = "postgres";
      user = "postgres";
      password = "";
      ssl_mode = Prefer;
      application_name = None;
      connect_timeout = Time.Duration.from_secs 10;
      keepalives_idle = None;
    }

  let from_string = fun str ->
    match Net.Uri.from_string str with
    | Ok uri when Net.Uri.scheme uri = Some "postgresql" || Net.Uri.scheme uri = Some "postgres" -> (
        let hostname =
          Net.Uri.host uri
          |> Option.unwrap_or ~default:"localhost"
        in
        let port =
          Net.Uri.port uri
          |> Option.unwrap_or ~default:5_432
        in
        let host =
          match Net.Addr.from_host_and_port ~host:hostname ~port with
          | Ok addr ->
              let resolved_ip = Net.Addr.ip addr in
              if resolved_ip = "::1" then
                "127.0.0.1"
              else
                resolved_ip
          | Error _ -> hostname
        in
        let path = Net.Uri.path uri in
        let database =
          if String.length path > 1 && String.get_unchecked path ~at:0 = '/' then
            String.sub path ~offset:1 ~len:(String.length path - 1)
          else
            "postgres"
        in
        match Net.Uri.authority uri with
        | Some auth_str -> (
            match String.split_on_char '@' auth_str with
            | [ userinfo; _ ] -> (
                match String.split_on_char ':' userinfo with
                | [ user; password ] ->
                    Ok {
                      host;
                      port;
                      database;
                      user;
                      password;
                      ssl_mode = Prefer;
                      application_name = None;
                      connect_timeout = Time.Duration.from_secs 10;
                      keepalives_idle = None;
                    }
                | [ user ] ->
                    Ok {
                      host;
                      port;
                      database;
                      user;
                      password = "";
                      ssl_mode = Prefer;
                      application_name = None;
                      connect_timeout = Time.Duration.from_secs 10;
                      keepalives_idle = None;
                    }
                | _ -> Error "Invalid userinfo format in URI"
              )
            | _ -> Error "Invalid authority format in URI"
          )
        | None -> Error "Missing user credentials in URI"
      )
    | Ok _ -> (
        match String.split_on_char ':' str with
        | [ host; port_str; database; user; password ] -> (
            match Int.parse port_str with
            | Some port ->
                Ok {
                  host;
                  port;
                  database;
                  user;
                  password;
                  ssl_mode = Prefer;
                  application_name = None;
                  connect_timeout = Time.Duration.from_secs 10;
                  keepalives_idle = None;
                }
            | None -> Error "Invalid port number"
          )
        | _ ->
            Error "Invalid connection string format (use \
               'postgresql://user:pass@host:port/db' or \
               'host:port:database:user:password')"
      )
    | Error _ -> Error "Failed to parse connection string"
end

module Driver = struct
  external hmac_sha256_bytes: string -> string -> bytes = "std_crypto_hmac_sha256"

  type config = Config.t

  (* Proper error type that distinguishes transport vs protocol errors *)

  type error =
    | TransportError of Net.TcpStream.error
    | ProtocolError of Protocol.Error.t
    | ConnectionClosed
    | AuthenticationNotSupported of string
    | TlsNotSupported of string
    | UnexpectedMessage of string

  type connection = {
    id: string;
    stream: Net.TcpStream.t option;
    config: config;
    mutable transaction_status: char;
    mutable closed: bool;
    prepared_statements: (string, statement) Collections.HashMap.t;
  }

  and statement = {
    name: string;
    sql: string;
    conn: connection;
  }

  type result_set = {
    rows: Sqlx_driver.Row.t Collections.Queue.t;
    mutable rows_affected: int;
  }

  let name = "PostgreSQL"

  let error_to_string = fun __tmp1 ->
    match __tmp1 with
    | TransportError Net.TcpStream.Connection_refused -> "Connection refused"
    | TransportError Net.TcpStream.Closed -> "Connection closed"
    | TransportError (Net.TcpStream.System_error io_err) ->
        "Transport error: " ^ IO.error_message io_err
    | ProtocolError proto_err -> Protocol.Error.to_string proto_err
    | ConnectionClosed -> "Connection is closed"
    | AuthenticationNotSupported method_ -> "Authentication method not supported: " ^ method_
    | TlsNotSupported mode -> "PostgreSQL TLS mode is not supported yet: " ^ mode
    | UnexpectedMessage msg -> "Unexpected message: " ^ msg

  let error_to_json = fun __tmp1 ->
    match __tmp1 with
    | TransportError Net.TcpStream.Connection_refused ->
        Data.Json.obj
          [
            ("type", Data.Json.string "transport_error");
            ("error", Data.Json.string "connection_refused");
            ("message", Data.Json.string "Connection refused");
          ]
    | TransportError Net.TcpStream.Closed ->
        Data.Json.obj
          [
            ("type", Data.Json.string "transport_error");
            ("error", Data.Json.string "closed");
            ("message", Data.Json.string "Connection closed");
          ]
    | TransportError (Net.TcpStream.System_error io_err) ->
        Data.Json.obj
          [
            ("type", Data.Json.string "transport_error");
            ("error", Data.Json.string "system_error");
            ("message", Data.Json.string (IO.error_message io_err));
          ]
    | ProtocolError proto_err -> Protocol.Error.to_json proto_err
    | ConnectionClosed ->
        Data.Json.obj
          [
            ("type", Data.Json.string "connection_closed");
            ("message", Data.Json.string "Connection is closed");
          ]
    | AuthenticationNotSupported method_ ->
        Data.Json.obj
          [
            ("type", Data.Json.string "authentication_not_supported");
            ("method", Data.Json.string method_);
            ("message", Data.Json.string ("Authentication method not supported: " ^ method_));
          ]
    | TlsNotSupported mode ->
        Data.Json.obj
          [
            ("type", Data.Json.string "tls_not_supported");
            ("mode", Data.Json.string mode);
            ("message", Data.Json.string ("PostgreSQL TLS mode is not supported yet: " ^ mode));
          ]
    | UnexpectedMessage msg ->
        Data.Json.obj
          [ ("type", Data.Json.string "unexpected_message"); ("message", Data.Json.string msg) ]

  let write_message = fun stream msg ->
    let bytes = Bytes.from_string msg in
    let total = Bytes.length bytes in
    let rec loop offset =
      if offset >= total then
        Ok ()
      else
        match Net.TcpStream.write stream bytes ~pos:offset ~len:(total - offset) () with
        | Error err -> Error (TransportError err)
        | Ok 0 -> Error (TransportError Net.TcpStream.Closed)
        | Ok written -> loop (offset + written)
    in
    loop 0

  let hmac_sha256 = fun ~key ~data ->
    hmac_sha256_bytes key data
    |> Bytes.to_string

  let sha256_bytes = fun data ->
    Crypto.Sha256.hash_string data
    |> Crypto.Digest.bytes
    |> Bytes.to_string

  let md5_hex = fun data ->
    Crypto.Md5.hash_string data
    |> Crypto.Digest.hex

  let xor_strings = fun left right ->
    let len = String.length left in
    let bytes = Bytes.create ~size:len in
    for index = 0 to len - 1 do
      let l = Char.code (String.get_unchecked left ~at:index) in
      let r = Char.code (String.get_unchecked right ~at:index) in
      Bytes.set_unchecked bytes ~at:index ~char:(Char.from_int_unchecked (l lxor r))
    done;
    Bytes.to_string bytes

  let int32_be = fun value ->
    let bytes = Bytes.create ~size:4 in
    Bytes.set_unchecked bytes ~at:0 ~char:(Char.from_int_unchecked ((value lsr 24) land 0xff));
    Bytes.set_unchecked bytes ~at:1 ~char:(Char.from_int_unchecked ((value lsr 16) land 0xff));
    Bytes.set_unchecked bytes ~at:2 ~char:(Char.from_int_unchecked ((value lsr 8) land 0xff));
    Bytes.set_unchecked bytes ~at:3 ~char:(Char.from_int_unchecked (value land 0xff));
    Bytes.to_string bytes

  let pbkdf2_sha256 = fun ~password ~salt ~iterations ->
    let first = hmac_sha256 ~key:password ~data:(salt ^ int32_be 1) in
    let output = Bytes.from_string first in
    let rec loop remaining previous =
      if remaining <= 1 then
        Bytes.to_string output
      else
        let next = hmac_sha256 ~key:password ~data:previous in
        for index = 0 to String.length next - 1 do
          let current = Char.code (Bytes.get_unchecked output ~at:index) in
          let update = Char.code (String.get_unchecked next ~at:index) in
          Bytes.set_unchecked output ~at:index ~char:(Char.from_int_unchecked (current lxor update))
        done;
      loop (remaining - 1) next
    in
    loop iterations first

  let sasl_escape = fun value ->
    let buffer = Buffer.create ~size:(String.length value) in
    String.for_each
      value
      ~fn:(fun char ->
        match char with
        | ',' -> Buffer.add_string buffer "=2C"
        | '=' -> Buffer.add_string buffer "=3D"
        | _ -> Buffer.add_char buffer char);
    Buffer.contents buffer

  let scram_nonce = fun () -> "berrybot-" ^ UUID.to_string (UUID.v7_monotonic ())

  let has_mechanism = fun mechanisms expected ->
    let rec loop mechanisms =
      match mechanisms with
      | [] -> false
      | mechanism :: rest -> mechanism = expected || loop rest
    in
    loop mechanisms

  let field_value = fun key fields ->
    let prefix = key ^ "=" in
    let rec loop fields =
      match fields with
      | [] -> None
      | field :: rest ->
          if String.starts_with ~prefix field then
            Some (String.sub
              field
              ~offset:(String.length prefix)
              ~len:(String.length field - String.length prefix))
          else
            loop rest
    in
    loop fields

  let parse_scram_attributes = fun payload -> String.split_on_char ',' payload

  let scram_client_final = fun (cfg: Config.t) client_first_bare server_first ->
    let fields = parse_scram_attributes server_first in
    match (field_value "r" fields, field_value "s" fields, field_value "i" fields) with
    | (Some server_nonce, Some salt_b64, Some iterations_text) -> (
        match (Int.parse iterations_text, Encoding.Base64.decode salt_b64) with
        | (Some iterations, Ok salt) when iterations > 0 ->
            let client_final_without_proof = "c=biws,r=" ^ server_nonce in
            let auth_message =
              client_first_bare ^ "," ^ server_first ^ "," ^ client_final_without_proof
            in
            let salted_password = pbkdf2_sha256 ~password:cfg.password ~salt ~iterations in
            let client_key = hmac_sha256 ~key:salted_password ~data:"Client Key" in
            let stored_key = sha256_bytes client_key in
            let client_signature = hmac_sha256 ~key:stored_key ~data:auth_message in
            let client_proof =
              xor_strings client_key client_signature
              |> Bytes.from_string
              |> Encoding.Base64.encode_bytes
            in
            let server_key = hmac_sha256 ~key:salted_password ~data:"Server Key" in
            let server_signature =
              hmac_sha256 ~key:server_key ~data:auth_message
              |> Bytes.from_string
              |> Encoding.Base64.encode_bytes
            in
            Ok (client_final_without_proof ^ ",p=" ^ client_proof, server_signature)
        | (None, _) ->
            Error (UnexpectedMessage "SCRAM server-first-message has invalid iteration count")
        | (Some _, Ok _) ->
            Error (UnexpectedMessage "SCRAM server-first-message has invalid iteration count")
        | (_, Error _) -> Error (UnexpectedMessage "SCRAM server-first-message has invalid salt")
      )
    | _ -> Error (UnexpectedMessage "SCRAM server-first-message is missing required fields")

  let verify_scram_final = fun expected_signature payload ->
    match field_value "v" (parse_scram_attributes payload) with
    | Some signature when signature = expected_signature -> Ok ()
    | Some _ -> Error (UnexpectedMessage "SCRAM server signature mismatch")
    | None -> Error (UnexpectedMessage "SCRAM server-final-message is missing verifier")

  let read_exact = fun stream buf len ->
    let rec loop offset remaining =
      if remaining = 0 then
        Ok ()
      else
        match Net.TcpStream.read stream buf ~pos:offset ~len:remaining () with
        | Error e -> Error e
        | Ok 0 -> Error Closed
        | Ok n -> loop (offset + n) (remaining - n)
    in
    loop 0 len

  let max_backend_message_body_length = 64 * 1_024 * 1_024

  let read_message = fun stream ->
    let header = Bytes.create ~size:5 in
    match read_exact stream header 5 with
    | Error err -> Error (TransportError err)
    | Ok () ->
        let msg_type = Char.code (Option.unwrap (Bytes.get header ~at:0)) in
        let b1 = Char.code (Option.unwrap (Bytes.get header ~at:1)) in
        let b2 = Char.code (Option.unwrap (Bytes.get header ~at:2)) in
        let b3 = Char.code (Option.unwrap (Bytes.get header ~at:3)) in
        let b4 = Char.code (Option.unwrap (Bytes.get header ~at:4)) in
        let length = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
        let body_len = length - 4 in
        if length < 4 then
          Error (UnexpectedMessage ("Invalid PostgreSQL backend message length: "
          ^ Int.to_string length))
        else if body_len > max_backend_message_body_length then
          Error (UnexpectedMessage ("PostgreSQL backend message exceeds maximum supported body length: "
          ^ Int.to_string body_len))
        else if body_len > 0 then
          let body = Bytes.create ~size:body_len in
          match read_exact stream body body_len with
          | Error err -> Error (TransportError err)
          | Ok () -> Ok (msg_type, length, body)
        else
          Ok (msg_type, length, Bytes.create ~size:0)

  let parse_backend_message = fun msg_type length body ->
    match Protocol.Reader.parse_backend_message_result msg_type length body with
    | Ok message -> Ok message
    | Error error ->
        Error (UnexpectedMessage ("Invalid PostgreSQL backend message: "
        ^ Protocol.Reader.parse_error_to_string error))

  let authenticate_cleartext = fun stream (cfg: Config.t) ->
    write_message
      stream
      (Protocol.Writer.password_message cfg.password)

  let authenticate_md5 = fun stream (cfg: Config.t) salt ->
    let salt_text = Bytes.to_string salt in
    let inner = md5_hex (cfg.password ^ cfg.user) in
    let password = "md5" ^ md5_hex (inner ^ salt_text) in
    write_message stream (Protocol.Writer.password_message password)

  let authenticate_scram_sha256 = fun stream (cfg: Config.t) mechanisms ->
    if not (has_mechanism mechanisms "SCRAM-SHA-256") then
      Error (AuthenticationNotSupported ("SASL mechanisms: " ^ String.concat "," mechanisms))
    else
      let client_nonce = scram_nonce () in
      let client_first_bare = "n=" ^ sasl_escape cfg.user ^ ",r=" ^ client_nonce in
      let client_first = "n,," ^ client_first_bare in
      match write_message
        stream
        (Protocol.Writer.sasl_initial_response ~mechanism:"SCRAM-SHA-256" ~response:client_first) with
      | Error error -> Error error
      | Ok () -> (
          match read_message stream with
          | Error error -> Error error
          | Ok (msg_type, length, body) -> (
              match parse_backend_message msg_type length body with
              | Error error -> Error error
              | Ok (Protocol.AuthenticationSASLContinue server_first) -> (
                  match scram_client_final cfg client_first_bare server_first with
                  | Error error -> Error error
                  | Ok (client_final, server_signature) -> (
                      match write_message stream (Protocol.Writer.sasl_response client_final) with
                      | Error error -> Error error
                      | Ok () -> (
                          match read_message stream with
                          | Error error -> Error error
                          | Ok (msg_type, length, body) -> (
                              match parse_backend_message msg_type length body with
                              | Error error -> Error error
                              | Ok (Protocol.AuthenticationSASLFinal server_final) ->
                                  verify_scram_final server_signature server_final
                              | Ok (Protocol.ErrorResponse err) -> Error (ProtocolError err)
                              | Ok _ -> Error (UnexpectedMessage "Expected SCRAM final message")
                            )
                        )
                    )
                )
              | Ok (Protocol.ErrorResponse err) -> Error (ProtocolError err)
              | Ok _ -> Error (UnexpectedMessage "Expected SCRAM continue message")
            )
        )

  let perform_handshake = fun stream (cfg: Config.t) ->
    let startup_msg =
      Protocol.Writer.startup_message
        ~user:cfg.user
        ~database:cfg.database
        ~application_name:cfg.application_name
    in
    match write_message stream startup_msg with
    | Error e -> Error e
    | Ok () ->
        let rec read_until_ready () =
          match read_message stream with
          | Error e -> Error e
          | Ok (msg_type, length, body) -> (
              match parse_backend_message msg_type length body with
              | Error error -> Error error
              | Ok backend_msg -> (
                  match backend_msg with
                  | Protocol.AuthenticationOk -> read_until_ready ()
                  | Protocol.AuthenticationCleartextPassword ->
                      authenticate_cleartext stream cfg
                      |> Result.and_then ~fn:read_until_ready
                  | Protocol.AuthenticationMD5Password salt ->
                      authenticate_md5 stream cfg salt
                      |> Result.and_then ~fn:read_until_ready
                  | Protocol.AuthenticationSASL mechanisms ->
                      authenticate_scram_sha256 stream cfg mechanisms
                      |> Result.and_then ~fn:read_until_ready
                  | Protocol.AuthenticationSASLContinue _ ->
                      Error (UnexpectedMessage "SCRAM continue without SASL start")
                  | Protocol.AuthenticationSASLFinal _ ->
                      Error (UnexpectedMessage "SCRAM final without SASL start")
                  | Protocol.ParameterStatus { name; value } ->
                      Log.debug ("PostgreSQL parameter: " ^ name ^ " = " ^ value);
                      read_until_ready ()
                  | Protocol.BackendKeyData { process_id; secret_key } ->
                      Log.debug
                        ("Backend key data: pid="
                        ^ string_of_int process_id
                        ^ " secret="
                        ^ string_of_int secret_key);
                      read_until_ready ()
                  | Protocol.ReadyForQuery status ->
                      Log.debug ("Ready for query, status: " ^ String.make ~len:1 ~char:status);
                      Ok ()
                  | Protocol.ErrorResponse err -> Error (ProtocolError err)
                  | Protocol.NoticeResponse err ->
                      Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                      read_until_ready ()
                  | _ ->
                      Error (UnexpectedMessage ("During handshake: "
                      ^ String.make ~len:1 ~char:(Char.from_int_unchecked msg_type)))
                )
            )
        in
        read_until_ready ()

  (* Initialize connection settings for consistent timestamp parsing *)

  let initialize_connection = fun stream ->
    (* Force ISO DateStyle for consistent timestamp format parsing *)
    let datestyle_msg = Protocol.Writer.query_message "SET DateStyle = 'ISO'" in
    (* Force UTC timezone to avoid ambiguity with TIMESTAMP (no TZ) *)
    let timezone_msg = Protocol.Writer.query_message "SET timezone = 'UTC'" in
    let rec drain_responses () =
      match read_message stream with
      | Error e -> Error e
      | Ok (msg_type, length, body) -> (
          match parse_backend_message msg_type length body with
          | Error error -> Error error
          | Ok backend_msg -> (
              match backend_msg with
              | Protocol.ReadyForQuery _ -> Ok ()
              | Protocol.ErrorResponse err -> Error (ProtocolError err)
              | _ -> drain_responses ()
            )
        )
    in
    match write_message stream datestyle_msg with
    | Error e -> Error e
    | Ok () -> (
        match drain_responses () with
        | Error e -> Error e
        | Ok () -> (
            match write_message stream timezone_msg with
            | Error e -> Error e
            | Ok () -> drain_responses ()
          )
      )

  let connect = fun (cfg: Config.t) ->
    let id =
      "pg_"
      ^ (
        string_of_int
          (
            Random.int 1_000_000
            |> Result.expect ~msg:"failed to generate client id"
          )
      )
    in
    match cfg.ssl_mode with
    | Require -> Error (TlsNotSupported "require")
    | Disable
    | Prefer -> (
        match Net.Addr.from_host_and_port ~host:cfg.host ~port:cfg.port with
        | Error (Net.Addr.System_error _err) ->
            (* Host resolution failure - treat as connection refused *)
            Error (TransportError Net.TcpStream.Connection_refused)
        | Error (Net.Addr.Invalid_port_number _ | Net.Addr.Invalid_format _) ->
            (* Invalid address format - treat as connection refused *)
            Error (TransportError Net.TcpStream.Connection_refused)
        | Ok addr -> (
            match Net.TcpStream.connect addr with
            | Error err -> Error (TransportError err)
            | Ok stream -> (
                match perform_handshake stream cfg with
                | Error e ->
                    Net.TcpStream.close stream;
                    Error e
                | Ok () -> (
                    match initialize_connection stream with
                    | Error e ->
                        Net.TcpStream.close stream;
                        Error e
                    | Ok () ->
                        Ok {
                          id;
                          stream = Some stream;
                          config = cfg;
                          transaction_status = 'I';
                          closed = false;
                          prepared_statements = Collections.HashMap.create ();
                        }
                  )
              )
          )
      )

  let close = fun conn ->
    conn.closed <- true;
    match conn.stream with
    | Some stream -> Net.TcpStream.close stream
    | None -> ()

  let ping = fun conn -> not conn.closed

  (* Strip timezone name suffix like " UTC", " EST", " PST" etc. *)

  let strip_timezone_name = fun str ->
    (* Look for last space that might separate offset from timezone name *)
    match String.last_index str ' ' with
    | Some idx when idx > 10 -> (
        (* Check if what comes after looks like a timezone abbreviation *)
        let after_space = String.sub str ~offset:(idx + 1) ~len:(String.length str - idx - 1) in
        (* Timezone names are usually 3-4 uppercase letters or contain '/' *)
        let is_tz_name =
          String.length after_space <= 5
          && String.for_all
            after_space
            ~fn:(fun c -> Char.uppercase_ascii c = c || c = '/' || c = '_')
        in
        if is_tz_name then
          String.sub str ~offset:0 ~len:idx
        else
          str
      )
    | _ -> str

  (* Parse PostgreSQL TIMESTAMP format: "2025-11-21 14:30:00.123456" *)

  let parse_pg_timestamp = fun str ->
    (* PostgreSQL uses space instead of 'T', so replace it for ISO8601 compatibility *)
    (* With timezone=UTC setting, TIMESTAMP values are already in UTC *)
    let iso_str =
      match String.index_of str ~char:' ' with
      | Some idx ->
          let before = String.sub str ~offset:0 ~len:idx in
          let after = String.sub str ~offset:(idx + 1) ~len:(String.length str - idx - 1) in
          before ^ "T" ^ after ^ "Z"
      | None -> str ^ "Z"
    in
    DateTime.parse iso_str

  (* Parse PostgreSQL TIMESTAMPTZ format: "2025-11-21 14:30:00.123456+00" or with TZ name *)

  let parse_pg_timestamptz = fun str ->
    (* First, strip any timezone name suffix like " UTC" *)
    let str = strip_timezone_name str in
    (* Replace space with 'T' for ISO8601 compatibility *)
    let iso_str =
      match String.index_of str ~char:' ' with
      | Some idx ->
          let before = String.sub str ~offset:0 ~len:idx in
          let after = String.sub str ~offset:(idx + 1) ~len:(String.length str - idx - 1) in
          before ^ "T" ^ after
      | None -> str
    in
    DateTime.parse iso_str

  let decode_value = fun (field: Protocol.Row.field) (value: string) ->
    match field.type_oid with
    | Protocol.TypeOid.Bool -> (
        match value with
        | "t" -> Sqlx_driver.Value.bool true
        | "f" -> Sqlx_driver.Value.bool false
        | _ -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int2 -> (
        match Int.parse value with
        | Some n -> Sqlx_driver.Value.int16 n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int4 -> (
        match Int.parse value with
        | Some n -> Sqlx_driver.Value.int n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int8 -> (
        match Int64.from_string_opt value with
        | Some n -> Sqlx_driver.Value.int64 n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Float4
    | Protocol.TypeOid.Float8 -> (
        match Float.parse value with
        | Some f -> Sqlx_driver.Value.float f
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Uuid -> Sqlx_driver.Value.uuid value
    | Protocol.TypeOid.Json
    | Protocol.TypeOid.Jsonb -> Sqlx_driver.Value.json value
    | Protocol.TypeOid.Numeric -> Sqlx_driver.Value.numeric value
    | Protocol.TypeOid.Timestamp -> (
        match parse_pg_timestamp value with
        | Ok dt -> Sqlx_driver.Value.timestamp dt
        | Error _ -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Timestamptz -> (
        match parse_pg_timestamptz value with
        | Ok dt -> Sqlx_driver.Value.timestamp_with_timezone dt
        | Error _ -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Text
    | Protocol.TypeOid.Varchar
    | Protocol.TypeOid.Char -> Sqlx_driver.Value.string value
    | Protocol.TypeOid.Bytea
    | Protocol.TypeOid.Oid
    | Protocol.TypeOid.Date
    | Protocol.TypeOid.Time
    | Protocol.TypeOid.Interval
    | Protocol.TypeOid.Unknown _ -> Sqlx_driver.Value.string value

  (* Format DateTime.t to PostgreSQL timestamp format *)

  let datetime_to_pg_format = fun dt ->
    let pad n width =
      let s = string_of_int n in
      String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
    in
    let (micros, _precision) = dt.DateTime.microseconds in
    (* Format: YYYY-MM-DD HH:MM:SS.microseconds *)
    pad dt.year 4
    ^ "-"
    ^ pad dt.month 2
    ^ "-"
    ^ pad dt.day 2
    ^ " "
    ^ pad dt.hour 2
    ^ ":"
    ^ pad dt.minute 2
    ^ ":"
    ^ pad dt.second 2
    ^ "."
    ^ pad micros 6

  let encode_param = fun (value: Sqlx_driver.Value.t) ->
    match value with
    | Null -> None
    | Int n -> Some (string_of_int n)
    | Int64 n -> Some (Int64.to_string n)
    | Int16 n -> Some (string_of_int n)
    | Float f -> Some (string_of_float f)
    | String s -> Some s
    | Bool true -> Some "t"
    | Bool false -> Some "f"
    | Bytes b -> Some (Bytes.to_string b)
    | Timestamp dt -> Some (datetime_to_pg_format dt)
    | TimestampWithTimezone dt -> Some (datetime_to_pg_format dt)
    | Date (y, m, d) ->
        let pad n width =
          let s = string_of_int n in
          String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
        in
        Some (pad y 4 ^ "-" ^ pad m 2 ^ "-" ^ pad d 2)
    | Time (h, min, s, us) ->
        let pad n width =
          let s = string_of_int n in
          String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
        in
        Some (pad h 2 ^ ":" ^ pad min 2 ^ ":" ^ pad s 2 ^ "." ^ pad us 6)
    | Uuid u -> Some u
    | Json j -> Some j
    | Numeric n -> Some n

  let prepare = fun conn sql ->
    if conn.closed then
      Error ConnectionClosed
    else
      let name =
        "stmt_"
        ^ string_of_int
          (
            Random.int 1_000_000
            |> Result.expect ~msg:"failed to generate statement id"
          )
      in
      let stmt = { name; sql; conn } in
      let _ = Collections.HashMap.insert conn.prepared_statements ~key:name ~value:stmt in
      ();
    Ok stmt

  let execute = fun stmt params ->
    if stmt.conn.closed then
      Error ConnectionClosed
    else
      match stmt.conn.stream with
      | None -> Error ConnectionClosed
      | Some stream -> (
          let use_extended_protocol = List.length params > 0 in
          if use_extended_protocol then
            let parse_msg =
              Protocol.Writer.parse_message
                ~statement_name:stmt.name
                ~query:stmt.sql
                ~param_types:[]
            in
            let describe_msg = Protocol.Writer.describe_message ~what:'S' ~name:stmt.name in
            let encoded_params = List.map params ~fn:encode_param in
            let bind_msg =
              Protocol.Writer.bind_message
                ~portal_name:""
                ~statement_name:stmt.name
                ~params:encoded_params
            in
            let execute_msg = Protocol.Writer.execute_message ~portal_name:"" ~max_rows:0 in
            let sync_msg = Protocol.Writer.sync_message () in
            let send_messages =
              let* () = write_message stream parse_msg in
              let* () = write_message stream describe_msg in
              let* () = write_message stream bind_msg in
              let* () = write_message stream execute_msg in
              write_message stream sync_msg
            in
            match send_messages with
            | Error e -> Error e
            | Ok () ->
                let result_set = { rows = Collections.Queue.create (); rows_affected = 0 } in
                let column_info = ref [] in
                let rec read_extended_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      match parse_backend_message msg_type length body with
                      | Error error -> Error error
                      | Ok backend_msg -> (
                          match backend_msg with
                          | Protocol.ParseComplete -> read_extended_results ()
                          | Protocol.BindComplete -> read_extended_results ()
                          | Protocol.RowDescription row_desc ->
                              column_info := row_desc;
                              read_extended_results ()
                          | Protocol.DataRow cols ->
                              let row =
                                if List.length !column_info = List.length cols then
                                  List.zip !column_info cols
                                  |> List.map
                                    ~fn:(fun ((field: Protocol.Row.field), row_val) ->
                                      let decoded_value =
                                        match row_val with
                                        | Protocol.Row.Null -> Sqlx_driver.Value.null
                                        | Protocol.Row.Value value -> decode_value field value
                                      in
                                      (field.name, decoded_value))
                                else
                                  List.enumerate cols
                                  |> List.map
                                    ~fn:(fun (index, row_val) ->
                                      let value =
                                        match row_val with
                                        | Protocol.Row.Null -> Sqlx_driver.Value.null
                                        | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                      in
                                      ("col_" ^ string_of_int index, value))
                              in
                              Collections.Queue.push result_set.rows ~value:row;
                              read_extended_results ()
                          | Protocol.CommandComplete tag ->
                              Log.debug ("Command complete: " ^ tag);
                              let parts = String.split_on_char ' ' tag in
                              (
                                match List.rev parts with
                                | n :: _ -> (
                                    match Int.parse n with
                                    | Some count -> result_set.rows_affected <- count
                                    | None -> ()
                                  )
                                | [] -> ()
                              );
                              read_extended_results ()
                          | Protocol.ReadyForQuery status ->
                              stmt.conn.transaction_status <- status;
                              Ok result_set
                          | Protocol.ErrorResponse err -> Error (ProtocolError err)
                          | Protocol.NoticeResponse err ->
                              Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                              read_extended_results ()
                          | Protocol.NoData -> read_extended_results ()
                          | Protocol.EmptyQueryResponse -> Ok result_set
                          | _ -> read_extended_results ()
                        )
                    )
                in
                read_extended_results ()
          else
            let query_msg = Protocol.Writer.query_message stmt.sql in
            match write_message stream query_msg with
            | Error e -> Error e
            | Ok () ->
                let result_set = { rows = Collections.Queue.create (); rows_affected = 0 } in
                let column_info = ref [] in
                let rec read_query_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      match parse_backend_message msg_type length body with
                      | Error error -> Error error
                      | Ok backend_msg -> (
                          match backend_msg with
                          | Protocol.RowDescription row_desc ->
                              column_info := row_desc;
                              read_query_results ()
                          | Protocol.DataRow cols ->
                              let row =
                                if List.length !column_info = List.length cols then
                                  List.zip !column_info cols
                                  |> List.map
                                    ~fn:(fun ((field: Protocol.Row.field), row_val) ->
                                      let decoded_value =
                                        match row_val with
                                        | Protocol.Row.Null -> Sqlx_driver.Value.null
                                        | Protocol.Row.Value value -> decode_value field value
                                      in
                                      (field.name, decoded_value))
                                else
                                  List.enumerate cols
                                  |> List.map
                                    ~fn:(fun (index, row_val) ->
                                      let value =
                                        match row_val with
                                        | Protocol.Row.Null -> Sqlx_driver.Value.null
                                        | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                      in
                                      ("col_" ^ string_of_int index, value))
                              in
                              Collections.Queue.push result_set.rows ~value:row;
                              read_query_results ()
                          | Protocol.CommandComplete tag ->
                              Log.debug ("Command complete: " ^ tag);
                              let parts = String.split_on_char ' ' tag in
                              (
                                match List.rev parts with
                                | n :: _ -> (
                                    match Int.parse n with
                                    | Some count -> result_set.rows_affected <- count
                                    | None -> ()
                                  )
                                | [] -> ()
                              );
                              read_query_results ()
                          | Protocol.ReadyForQuery status ->
                              stmt.conn.transaction_status <- status;
                              Ok result_set
                          | Protocol.ErrorResponse err -> Error (ProtocolError err)
                          | Protocol.NoticeResponse err ->
                              Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                              read_query_results ()
                          | _ ->
                              Error (UnexpectedMessage ("During query: "
                              ^ String.make ~len:1 ~char:(Char.from_int_unchecked msg_type)))
                        )
                    )
                in
                read_query_results ()
        )

  let fetch_row = fun result_set -> Collections.Queue.pop result_set.rows

  let rows_affected = fun result_set -> result_set.rows_affected

  let execute_simple_command = fun conn sql ->
    match prepare conn sql with
    | Error error -> Error error
    | Ok stmt -> (
        match execute stmt [] with
        | Error error -> Error error
        | Ok _ -> Ok ()
      )

  let begin_transaction = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.transaction_status != 'I' then
      Error (UnexpectedMessage "Transaction already in progress")
    else
      execute_simple_command conn "BEGIN"

  let commit = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.transaction_status != 'T' then
      Error (UnexpectedMessage "No transaction in progress")
    else
      execute_simple_command conn "COMMIT"

  let rollback = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.transaction_status != 'T' then
      Error (UnexpectedMessage "No transaction in progress")
    else
      execute_simple_command conn "ROLLBACK"

  let isolation_level_sql = fun __tmp1 ->
    match __tmp1 with
    | `Read_uncommitted -> "READ UNCOMMITTED"
    | `Read_committed -> "READ COMMITTED"
    | `Repeatable_read -> "REPEATABLE READ"
    | `Serializable -> "SERIALIZABLE"

  let set_isolation_level = fun conn level ->
    if conn.closed then
      Error ConnectionClosed
    else
      let sql =
        if conn.transaction_status = 'T' then
          "SET TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
        else
          "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL " ^ isolation_level_sql level
      in
      execute_simple_command conn sql
end
