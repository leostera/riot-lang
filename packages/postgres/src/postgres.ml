open Std
open Std.IO
module Error = Protocol.Error

module Config = struct
  type ssl_mode =
    Disable
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
    match Net.Uri.of_string str with
    | Ok uri when Net.Uri.scheme uri = Some "postgresql" || Net.Uri.scheme uri = Some "postgres" -> (
        let hostname = Net.Uri.host uri |> Option.unwrap_or ~default:"localhost" in
        let port = Net.Uri.port uri |> Option.unwrap_or ~default:5_432 in
        let host =
          match Net.Addr.of_host_and_port ~host:hostname ~port with
          | Ok (`Tcp (resolved_ip, _)) ->
              if resolved_ip = "::1" then
                "127.0.0.1"
              else
                resolved_ip
          | Error _ -> hostname
        in
        let path = Net.Uri.path uri in
        let database =
          if String.length path > 1 && path.[0] = '/' then
            String.sub path 1 (String.length path - 1)
          else
            "postgres"
        in
        match Net.Uri.authority uri with
        | Some auth_str -> (
            match String.split_on_char '@' auth_str with
            | [userinfo;_] -> (
                match String.split_on_char ':' userinfo with
                | [user;password] ->
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
        | [host;port_str;database;user;password] -> (
            match int_of_string_opt port_str with
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
    | Error _ ->
        Error "Failed to parse connection string"
end

module Driver = struct
  type config = Config.t

  (* Proper error type that distinguishes transport vs protocol errors *)

  type error =
    | TransportError of Net.TcpStream.error
    | ProtocolError of Protocol.Error.t
    | ConnectionClosed
    | AuthenticationNotSupported of string
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

  let error_to_string = function
    | TransportError Net.TcpStream.Connection_refused -> "Connection refused"
    | TransportError Net.TcpStream.Closed -> "Connection closed"
    | TransportError (Net.TcpStream.System_error io_err) -> "Transport error: " ^ IO.error_message io_err
    | ProtocolError proto_err -> Protocol.Error.to_string proto_err
    | ConnectionClosed -> "Connection is closed"
    | AuthenticationNotSupported method_ -> "Authentication method not supported: " ^ method_
    | UnexpectedMessage msg -> "Unexpected message: " ^ msg

  let error_to_json = function
    | TransportError Net.TcpStream.Connection_refused -> Data.Json.obj
      [
        ("type", Data.Json.string "transport_error");
        ("error", Data.Json.string "connection_refused");
        ("message", Data.Json.string "Connection refused")
      ]
    | TransportError Net.TcpStream.Closed -> Data.Json.obj
      [
        ("type", Data.Json.string "transport_error");
        ("error", Data.Json.string "closed");
        ("message", Data.Json.string "Connection closed")
      ]
    | TransportError (Net.TcpStream.System_error io_err) -> Data.Json.obj
      [
        ("type", Data.Json.string "transport_error");
        ("error", Data.Json.string "system_error");
        ("message", Data.Json.string (IO.error_message io_err))
      ]
    | ProtocolError proto_err -> Protocol.Error.to_json proto_err
    | ConnectionClosed -> Data.Json.obj
      [
        ("type", Data.Json.string "connection_closed");
        ("message", Data.Json.string "Connection is closed")
      ]
    | AuthenticationNotSupported method_ -> Data.Json.obj
      [
        ("type", Data.Json.string "authentication_not_supported");
        ("method", Data.Json.string method_);
        ("message", Data.Json.string ("Authentication method not supported: " ^ method_))
      ]
    | UnexpectedMessage msg -> Data.Json.obj
      [ ("type", Data.Json.string "unexpected_message"); ("message", Data.Json.string msg) ]

  let write_message = fun stream msg ->
    let bytes = Bytes.of_string msg in
    match Net.TcpStream.write stream bytes () with
    | Error err -> Error (TransportError err)
    | Ok _n -> Ok ()

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

  let read_message = fun stream ->
    let header = Bytes.create 5 in
    match read_exact stream header 5 with
    | Error err -> Error (TransportError err)
    | Ok () ->
        let msg_type = Char.code (Bytes.get header 0) in
        let b1 = Char.code (Bytes.get header 1) in
        let b2 = Char.code (Bytes.get header 2) in
        let b3 = Char.code (Bytes.get header 3) in
        let b4 = Char.code (Bytes.get header 4) in
        let length = (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4 in
        let body_len = length - 4 in
        if body_len > 0 then
          let body = Bytes.create body_len in
          match read_exact stream body body_len with
          | Error err -> Error (TransportError err)
          | Ok () -> Ok (msg_type, length, body)
        else
          Ok (msg_type, length, Bytes.create 0)

  let perform_handshake = fun stream (cfg: Config.t) ->
    let startup_msg = Protocol.Writer.startup_message
      ~user:cfg.user
      ~database:cfg.database
      ~application_name:cfg.application_name in
    match write_message stream startup_msg with
    | Error e -> Error e
    | Ok () ->
        let rec read_until_ready () =
          match read_message stream with
          | Error e -> Error e
          | Ok (msg_type, length, body) -> (
              let backend_msg = Protocol.Reader.parse_backend_message msg_type length body in
              match backend_msg with
              | Protocol.AuthenticationOk ->
                  read_until_ready ()
              | Protocol.AuthenticationCleartextPassword ->
                  Error (AuthenticationNotSupported "Cleartext password")
              | Protocol.AuthenticationMD5Password _ ->
                  Error (AuthenticationNotSupported "MD5 password")
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
                  Log.debug ("Ready for query, status: " ^ String.make 1 status);
                  Ok ()
              | Protocol.ErrorResponse err ->
                  Error (ProtocolError err)
              | Protocol.NoticeResponse err ->
                  Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                  read_until_ready ()
              | _ ->
                  Error (UnexpectedMessage ("During handshake: " ^ String.make 1 (Char.chr msg_type)))
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
          let backend_msg = Protocol.Reader.parse_backend_message msg_type length body in
          match backend_msg with
          | Protocol.ReadyForQuery _ -> Ok ()
          | Protocol.ErrorResponse err -> Error (ProtocolError err)
          | _ -> drain_responses ()
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
    let id = "pg_" ^ string_of_int (Random.int 1_000_000) in
    match Net.Addr.of_host_and_port ~host:cfg.host ~port:cfg.port with
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

  let close = fun conn ->
    conn.closed <- true;
    match conn.stream with
    | Some stream -> Net.TcpStream.close stream
    | None -> ()

  let ping = fun conn -> not conn.closed

  (* Strip timezone name suffix like " UTC", " EST", " PST" etc. *)

  let strip_timezone_name = fun str ->
    (* Look for last space that might separate offset from timezone name *)
    match String.rindex_opt str ' ' with
    | Some idx when idx > 10 -> (
        (* Check if what comes after looks like a timezone abbreviation *)
        let after_space = String.sub str (idx + 1) (String.length str - idx - 1) in
        (* Timezone names are usually 3-4 uppercase letters or contain '/' *)
        let is_tz_name =
          String.length after_space <= 5
          && String.for_all (fun c -> Char.uppercase_ascii c = c || c = '/' || c = '_') after_space in
        if is_tz_name then
          String.sub str 0 idx
        else
          str
      )
    | _ -> str

  (* Parse PostgreSQL TIMESTAMP format: "2025-11-21 14:30:00.123456" *)

  let parse_pg_timestamp = fun str ->
    (* PostgreSQL uses space instead of 'T', so replace it for ISO8601 compatibility *)
    (* With timezone=UTC setting, TIMESTAMP values are already in UTC *)
    let iso_str =
      match String.index_opt str ' ' with
      | Some idx ->
          let before = String.sub str 0 idx in
          let after = String.sub str (idx + 1) (String.length str - idx - 1) in
          before ^ "T" ^ after ^ "Z"
      | None -> str ^ "Z"
    in
    Datetime.parse iso_str

  (* Parse PostgreSQL TIMESTAMPTZ format: "2025-11-21 14:30:00.123456+00" or with TZ name *)

  let parse_pg_timestamptz = fun str ->
    (* First, strip any timezone name suffix like " UTC" *)
    let str = strip_timezone_name str in
    (* Replace space with 'T' for ISO8601 compatibility *)
    let iso_str =
      match String.index_opt str ' ' with
      | Some idx ->
          let before = String.sub str 0 idx in
          let after = String.sub str (idx + 1) (String.length str - idx - 1) in
          before ^ "T" ^ after
      | None -> str
    in
    Datetime.parse iso_str

  let decode_value = fun (field: Protocol.Row.field) (value: string) ->
    match field.type_oid with
    | Protocol.TypeOid.Bool -> (
        match value with
        | "t" -> Sqlx_driver.Value.bool true
        | "f" -> Sqlx_driver.Value.bool false
        | _ -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int2 -> (
        match int_of_string_opt value with
        | Some n -> Sqlx_driver.Value.int16 n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int4 -> (
        match int_of_string_opt value with
        | Some n -> Sqlx_driver.Value.int n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Int8 -> (
        match Int64.of_string_opt value with
        | Some n -> Sqlx_driver.Value.int64 n
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Float4
    | Protocol.TypeOid.Float8 -> (
        match float_of_string_opt value with
        | Some f -> Sqlx_driver.Value.float f
        | None -> Sqlx_driver.Value.string value
      )
    | Protocol.TypeOid.Uuid ->
        Sqlx_driver.Value.uuid value
    | Protocol.TypeOid.Json
    | Protocol.TypeOid.Jsonb ->
        Sqlx_driver.Value.json value
    | Protocol.TypeOid.Numeric ->
        Sqlx_driver.Value.numeric value
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
    | Protocol.TypeOid.Char ->
        Sqlx_driver.Value.string value
    | Protocol.TypeOid.Bytea
    | Protocol.TypeOid.Oid
    | Protocol.TypeOid.Date
    | Protocol.TypeOid.Time
    | Protocol.TypeOid.Interval
    | Protocol.TypeOid.Unknown _ ->
        Sqlx_driver.Value.string value

  (* Format Datetime.t to PostgreSQL timestamp format *)

  let datetime_to_pg_format = fun dt ->
    let pad n width =
      let s = string_of_int n in
      String.make (max 0 (width - String.length s)) '0' ^ s
    in
    let micros, _precision = dt.Datetime.microseconds in
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
    | Null ->
        ""
    | Int n ->
        string_of_int n
    | Int64 n ->
        Int64.to_string n
    | Int16 n ->
        string_of_int n
    | Float f ->
        string_of_float f
    | String s ->
        s
    | Bool true ->
        "t"
    | Bool false ->
        "f"
    | Bytes b ->
        Bytes.to_string b
    | Timestamp dt ->
        datetime_to_pg_format dt
    | TimestampWithTimezone dt ->
        datetime_to_pg_format dt
    | Date (y, m, d) ->
        let pad n width =
          let s = string_of_int n in
          String.make (max 0 (width - String.length s)) '0' ^ s
        in
        pad y 4 ^ "-" ^ pad m 2 ^ "-" ^ pad d 2
    | Time (h, min, s, us) ->
        let pad n width =
          let s = string_of_int n in
          String.make (max 0 (width - String.length s)) '0' ^ s
        in
        pad h 2 ^ ":" ^ pad min 2 ^ ":" ^ pad s 2 ^ "." ^ pad us 6
    | Uuid u ->
        u
    | Json j ->
        j
    | Numeric n ->
        n

  let prepare = fun conn sql ->
    if conn.closed then
      Error ConnectionClosed
    else
      let name = "stmt_" ^ string_of_int (Random.int 1_000_000) in
      let stmt = {name;sql;conn;} in
      Collections.HashMap.insert conn.prepared_statements name stmt |> ignore;
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
            let parse_msg = Protocol.Writer.parse_message
              ~statement_name:stmt.name
              ~query:stmt.sql
              ~param_types:[] in
            let describe_msg = Protocol.Writer.describe_message ~what:'S' ~name:stmt.name in
            let encoded_params = List.map encode_param params in
            let bind_msg = Protocol.Writer.bind_message
              ~portal_name:""
              ~statement_name:stmt.name
              ~params:encoded_params in
            let execute_msg = Protocol.Writer.execute_message ~portal_name:"" ~max_rows:0 in
            let sync_msg = Protocol.Writer.sync_message () in
            match (
              write_message stream parse_msg,
              write_message stream describe_msg,
              write_message stream bind_msg,
              write_message stream execute_msg,
              write_message stream sync_msg
            ) with
            | (Error e, _, _, _, _)
            | (_, Error e, _, _, _)
            | (_, _, Error e, _, _)
            | (_, _, _, Error e, _)
            | (_, _, _, _, Error e) -> Error e
            | Ok (), Ok (), Ok (), Ok (), Ok () ->
                let result_set = {rows = Collections.Queue.create ();rows_affected = 0;} in
                let column_info = ref [] in
                let rec read_extended_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      let backend_msg = Protocol.Reader.parse_backend_message msg_type length body in
                      match backend_msg with
                      | Protocol.ParseComplete ->
                          read_extended_results ()
                      | Protocol.BindComplete ->
                          read_extended_results ()
                      | Protocol.RowDescription row_desc ->
                          column_info := row_desc;
                          read_extended_results ()
                      | Protocol.DataRow cols ->
                          let row =
                            if List.length !column_info = List.length cols then
                              List.map2
                                (fun (field: Protocol.Row.field) row_val ->
                                  let decoded_value =
                                    match row_val with
                                    | Protocol.Row.Null -> Sqlx_driver.Value.null
                                    | Protocol.Row.Value value -> decode_value field value
                                  in
                                  (field.name, decoded_value))
                                !column_info
                                cols
                            else
                              List.mapi
                                (fun i row_val ->
                                  let value =
                                    match row_val with
                                    | Protocol.Row.Null -> Sqlx_driver.Value.null
                                    | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                  in
                                  ("col_" ^ string_of_int i, value))
                                cols
                          in
                          Collections.Queue.push result_set.rows row;
                          read_extended_results ()
                      | Protocol.CommandComplete tag ->
                          Log.debug ("Command complete: " ^ tag);
                          let parts = String.split_on_char ' ' tag in
                          (
                            match List.rev parts with
                            | n :: _ -> (
                                match int_of_string_opt n with
                                | Some count -> result_set.rows_affected <- count
                                | None -> ()
                              )
                            | [] -> ()
                          );
                          read_extended_results ()
                      | Protocol.ReadyForQuery _status ->
                          Ok result_set
                      | Protocol.ErrorResponse err ->
                          Error (ProtocolError err)
                      | Protocol.NoticeResponse err ->
                          Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                          read_extended_results ()
                      | Protocol.NoData ->
                          read_extended_results ()
                      | Protocol.EmptyQueryResponse ->
                          Ok result_set
                      | _ ->
                          read_extended_results ()
                    )
                in
                read_extended_results ()
          else
            let query_msg = Protocol.Writer.query_message stmt.sql in
            match write_message stream query_msg with
            | Error e -> Error e
            | Ok () ->
                let result_set = {rows = Collections.Queue.create ();rows_affected = 0;} in
                let column_info = ref [] in
                let rec read_query_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      let backend_msg = Protocol.Reader.parse_backend_message msg_type length body in
                      match backend_msg with
                      | Protocol.RowDescription row_desc ->
                          column_info := row_desc;
                          read_query_results ()
                      | Protocol.DataRow cols ->
                          let row =
                            if List.length !column_info = List.length cols then
                              List.map2
                                (fun (field: Protocol.Row.field) row_val ->
                                  let decoded_value =
                                    match row_val with
                                    | Protocol.Row.Null -> Sqlx_driver.Value.null
                                    | Protocol.Row.Value value -> decode_value field value
                                  in
                                  (field.name, decoded_value))
                                !column_info
                                cols
                            else
                              List.mapi
                                (fun i row_val ->
                                  let value =
                                    match row_val with
                                    | Protocol.Row.Null -> Sqlx_driver.Value.null
                                    | Protocol.Row.Value v -> Sqlx_driver.Value.string v
                                  in
                                  ("col_" ^ string_of_int i, value))
                                cols
                          in
                          Collections.Queue.push result_set.rows row;
                          read_query_results ()
                      | Protocol.CommandComplete tag ->
                          Log.debug ("Command complete: " ^ tag);
                          let parts = String.split_on_char ' ' tag in
                          (
                            match List.rev parts with
                            | n :: _ -> (
                                match int_of_string_opt n with
                                | Some count -> result_set.rows_affected <- count
                                | None -> ()
                              )
                            | [] -> ()
                          );
                          read_query_results ()
                      | Protocol.ReadyForQuery _status ->
                          Ok result_set
                      | Protocol.ErrorResponse err ->
                          Error (ProtocolError err)
                      | Protocol.NoticeResponse err ->
                          Log.info ("PostgreSQL notice: " ^ Protocol.Error.message err);
                          read_query_results ()
                      | _ ->
                          Error (UnexpectedMessage ("During query: "
                          ^ String.make 1 (Char.chr msg_type)))
                    )
                in
                read_query_results ()
        )

  let fetch_row = fun result_set -> Collections.Queue.pop result_set.rows

  let rows_affected = fun result_set -> result_set.rows_affected

  let begin_transaction = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else (
      conn.transaction_status <- 'T';
      Ok ()
    )

  let commit = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.transaction_status != 'T' then
      Error (UnexpectedMessage "No transaction in progress")
    else (
      conn.transaction_status <- 'I';
      Ok ()
    )

  let rollback = fun conn ->
    if conn.closed then
      Error ConnectionClosed
    else if conn.transaction_status != 'T' then
      Error (UnexpectedMessage "No transaction in progress")
    else (
      conn.transaction_status <- 'I';
      Ok ()
    )

  let set_isolation_level = fun _conn ->
    function
    | `Read_uncommitted -> Ok ()
    | `Read_committed -> Ok ()
    | `Repeatable_read -> Ok ()
    | `Serializable -> Ok ()
end
