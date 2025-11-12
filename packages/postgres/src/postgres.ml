open Std
open Std.IO

module Config = struct
  type t = {
    host : string;
    port : int;
    database : string;
    user : string;
    password : string;
    ssl_mode : [ `Disable | `Require | `Prefer ];
    application_name : string option;
    connect_timeout : Time.Duration.t;
    keepalives_idle : Time.Duration.t option;
  }

  let default () =
    {
      host = "localhost";
      port = 5432;
      database = "postgres";
      user = "postgres";
      password = "";
      ssl_mode = `Prefer;
      application_name = None;
      connect_timeout = Time.Duration.from_secs 10;
      keepalives_idle = None;
    }

  let from_string str =
    match Net.Uri.of_string str with
    | Ok uri
      when Net.Uri.scheme uri = Some "postgresql"
           || Net.Uri.scheme uri = Some "postgres" -> (
        let hostname =
          Net.Uri.host uri |> Option.unwrap_or ~default:"localhost"
        in
        let port = Net.Uri.port uri |> Option.unwrap_or ~default:5432 in

        let host =
          match Net.Addr.of_host_and_port ~host:hostname ~port with
          | Ok (`Tcp (resolved_ip, _)) ->
              if resolved_ip = "::1" then "127.0.0.1" else resolved_ip
          | Error _ -> hostname
        in
        let path = Net.Uri.path uri in
        let database =
          if String.length path > 1 && path.[0] = '/' then
            String.sub path 1 (String.length path - 1)
          else "postgres"
        in
        match Net.Uri.authority uri with
        | Some auth_str -> (
            match String.split_on_char '@' auth_str with
            | [ userinfo; _ ] -> (
                match String.split_on_char ':' userinfo with
                | [ user; password ] ->
                    Ok
                      {
                        host;
                        port;
                        database;
                        user;
                        password;
                        ssl_mode = `Prefer;
                        application_name = None;
                        connect_timeout = Time.Duration.from_secs 10;
                        keepalives_idle = None;
                      }
                | [ user ] ->
                    Ok
                      {
                        host;
                        port;
                        database;
                        user;
                        password = "";
                        ssl_mode = `Prefer;
                        application_name = None;
                        connect_timeout = Time.Duration.from_secs 10;
                        keepalives_idle = None;
                      }
                | _ -> Error "Invalid userinfo format in URI")
            | _ -> Error "Invalid authority format in URI")
        | None -> Error "Missing user credentials in URI")
    | Ok _ -> (
        match String.split_on_char ':' str with
        | [ host; port_str; database; user; password ] -> (
            match int_of_string_opt port_str with
            | Some port ->
                Ok
                  {
                    host;
                    port;
                    database;
                    user;
                    password;
                    ssl_mode = `Prefer;
                    application_name = None;
                    connect_timeout = Time.Duration.from_secs 10;
                    keepalives_idle = None;
                  }
            | None -> Error "Invalid port number")
        | _ ->
            Error
              "Invalid connection string format (use \
               'postgresql://user:pass@host:port/db' or \
               'host:port:database:user:password')")
    | Error _ -> Error "Failed to parse connection string"
end

module Driver = struct
  type config = Config.t

  type connection = {
    id : string;
    stream : Net.TcpStream.t option;
    config : config;
    mutable transaction_status : char;
    mutable closed : bool;
    prepared_statements : (string, statement) Collections.HashMap.t;
  }

  and statement = { name : string; sql : string; conn : connection }

  type result_set = {
    rows : Sqlx_driver.Row.t Collections.Queue.t;
    mutable rows_affected : int;
  }

  let name = "PostgreSQL"

  let write_message stream msg =
    let bytes = Bytes.of_string msg in
    match Net.TcpStream.write stream bytes () with
    | Error Connection_refused -> Error "Write error: Connection refused"
    | Error Closed -> Error "Write error: Connection closed"
    | Error (System_error msg) -> Error ("Write error: " ^ msg)
    | Ok _n -> Ok ()

  let read_exact stream buf len =
    let rec loop offset remaining =
      if remaining = 0 then Ok ()
      else
        match Net.TcpStream.read stream buf ~pos:offset ~len:remaining () with
        | Error e -> Error e
        | Ok 0 -> Error Closed
        | Ok n -> loop (offset + n) (remaining - n)
    in
    loop 0 len

  let read_message stream =
    let header = Bytes.create 5 in
    match read_exact stream header 5 with
    | Error Connection_refused -> Error "Read error: Connection refused"
    | Error Closed -> Error "Read error: Connection closed"
    | Error (System_error msg) -> Error ("Read error: " ^ msg)
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
          | Error Connection_refused ->
              Error "Read body error: Connection refused"
          | Error Closed -> Error "Read body error: Connection closed"
          | Error (System_error msg) ->
              Error ("Read body error: " ^ msg)
          | Ok () -> Ok (msg_type, length, body)
        else Ok (msg_type, length, Bytes.create 0)

  let perform_handshake stream (cfg : Config.t) =
    let startup_msg =
      Protocol.Writer.startup_message ~user:cfg.user ~database:cfg.database
        ~application_name:cfg.application_name
    in

    match write_message stream startup_msg with
    | Error e -> Error e
    | Ok () ->
        let rec read_until_ready () =
          match read_message stream with
          | Error e -> Error e
          | Ok (msg_type, length, body) -> (
              let backend_msg =
                Protocol.Reader.parse_backend_message msg_type length body
              in
              match backend_msg with
              | Protocol.AuthenticationOk -> read_until_ready ()
              | Protocol.AuthenticationCleartextPassword ->
                  Error "Cleartext password authentication not yet implemented"
              | Protocol.AuthenticationMD5Password _ ->
                  Error "MD5 password authentication not yet implemented"
              | Protocol.ParameterStatus { name; value } ->
                  Log.debug ("PostgreSQL parameter: " ^ name ^ " = " ^ value);
                  read_until_ready ()
              | Protocol.BackendKeyData { process_id; secret_key } ->
                  Log.debug ("Backend key data: pid=" ^ string_of_int process_id ^
                    " secret=" ^ string_of_int secret_key);
                  read_until_ready ()
              | Protocol.ReadyForQuery status ->
                  Log.debug ("Ready for query, status: " ^ String.make 1 status);
                  Ok ()
              | Protocol.ErrorResponse fields ->
                  let msg =
                    List.assoc_opt 'M' fields
                    |> Option.unwrap_or ~default:"Unknown error"
                  in
                  Error ("PostgreSQL error: " ^ msg)
              | Protocol.NoticeResponse fields ->
                  let msg =
                    List.assoc_opt 'M' fields |> Option.unwrap_or ~default:""
                  in
                  Log.info ("PostgreSQL notice: " ^ msg);
                  read_until_ready ()
              | _ ->
                  Error
                    ("Unexpected message during handshake: " ^
                       String.make 1 (Char.chr msg_type)))
        in
        read_until_ready ()

  let connect (cfg : Config.t) =
    let id = "pg_" ^ string_of_int (Random.int 1000000) in

    match Net.Addr.of_host_and_port ~host:cfg.host ~port:cfg.port with
    | Error (`System_error msg) ->
        Error ("Failed to resolve host " ^ cfg.host ^ ": " ^ msg)
    | Ok addr -> (
        match Net.TcpStream.connect addr with
        | Error Connection_refused ->
            Error ("Connection refused to " ^ cfg.host ^ ":" ^ string_of_int cfg.port)
        | Error Closed -> Error "Connection closed unexpectedly"
        | Error (System_error msg) -> Error ("System error: " ^ msg)
        | Ok stream -> (
            match perform_handshake stream cfg with
            | Error e ->
                Net.TcpStream.close stream;
                Error e
            | Ok () ->
                Ok
                  {
                    id;
                    stream = Some stream;
                    config = cfg;
                    transaction_status = 'I';
                    closed = false;
                    prepared_statements = Collections.HashMap.create ();
                  }))

  let close conn =
    conn.closed <- true;
    match conn.stream with
    | Some stream -> Net.TcpStream.close stream
    | None -> ()

  let ping conn = not conn.closed

  let decode_value (field : Protocol.field) (value : string) =
    match field.type_oid with
    | oid when oid = Protocol.TypeOid.bool -> (
        match value with
        | "t" -> Sqlx_driver.Value.bool true
        | "f" -> Sqlx_driver.Value.bool false
        | _ -> Sqlx_driver.Value.string value)
    | oid when oid = Protocol.TypeOid.int2 -> (
        match int_of_string_opt value with
        | Some n -> Sqlx_driver.Value.int16 n
        | None -> Sqlx_driver.Value.string value)
    | oid when oid = Protocol.TypeOid.int4 -> (
        match int_of_string_opt value with
        | Some n -> Sqlx_driver.Value.int n
        | None -> Sqlx_driver.Value.string value)
    | oid when oid = Protocol.TypeOid.int8 -> (
        match Int64.of_string_opt value with
        | Some n -> Sqlx_driver.Value.int64 n
        | None -> Sqlx_driver.Value.string value)
    | oid when oid = Protocol.TypeOid.float4 || oid = Protocol.TypeOid.float8
      -> (
        match float_of_string_opt value with
        | Some f -> Sqlx_driver.Value.float f
        | None -> Sqlx_driver.Value.string value)
    | oid when oid = Protocol.TypeOid.uuid -> Sqlx_driver.Value.uuid value
    | oid when oid = Protocol.TypeOid.json || oid = Protocol.TypeOid.jsonb ->
        Sqlx_driver.Value.json value
    | oid when oid = Protocol.TypeOid.numeric -> Sqlx_driver.Value.numeric value
    | oid
      when oid = Protocol.TypeOid.text
           || oid = Protocol.TypeOid.varchar
           || oid = Protocol.TypeOid.char ->
        Sqlx_driver.Value.string value
    | _ -> Sqlx_driver.Value.string value

  let encode_param (value : Sqlx_driver.Value.t) =
    match value with
    | Null -> ""
    | Int n -> string_of_int n
    | Int64 n -> Int64.to_string n
    | Int16 n -> string_of_int n
    | Float f -> string_of_float f
    | String s -> s
    | Bool true -> "t"
    | Bool false -> "f"
    | Bytes b -> Bytes.to_string b
    | Timestamp _t -> ""
    | TimestampWithTimezone _ -> ""
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
    | Uuid u -> u
    | Json j -> j
    | Numeric n -> n

  let prepare conn sql =
    if conn.closed then Error "Connection is closed"
    else
      let name = "stmt_" ^ string_of_int (Random.int 1000000) in
      let stmt = { name; sql; conn } in
      Collections.HashMap.insert conn.prepared_statements name stmt |> ignore;
      Ok stmt

  let execute stmt params =
    if stmt.conn.closed then Error "Connection is closed"
    else
      match stmt.conn.stream with
      | None -> Error "No active stream"
      | Some stream -> (
          let use_extended_protocol = List.length params > 0 in

          if use_extended_protocol then
            let parse_msg =
              Protocol.Writer.parse_message ~statement_name:stmt.name
                ~query:stmt.sql ~param_types:[]
            in
            let describe_msg =
              Protocol.Writer.describe_message ~what:'S' ~name:stmt.name
            in
            let encoded_params = List.map encode_param params in
            let bind_msg =
              Protocol.Writer.bind_message ~portal_name:""
                ~statement_name:stmt.name ~params:encoded_params
            in
            let execute_msg =
              Protocol.Writer.execute_message ~portal_name:"" ~max_rows:0
            in
            let sync_msg = Protocol.Writer.sync_message () in

            match
              ( write_message stream parse_msg,
                write_message stream describe_msg,
                write_message stream bind_msg,
                write_message stream execute_msg,
                write_message stream sync_msg )
            with
            | Error e, _, _, _, _
            | _, Error e, _, _, _
            | _, _, Error e, _, _
            | _, _, _, Error e, _
            | _, _, _, _, Error e ->
                Error e
            | Ok (), Ok (), Ok (), Ok (), Ok () ->
                let result_set =
                  { rows = Collections.Queue.create (); rows_affected = 0 }
                in
                let column_info = ref [] in
                let rec read_extended_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      let backend_msg =
                        Protocol.Reader.parse_backend_message msg_type length
                          body
                      in
                      match backend_msg with
                      | Protocol.ParseComplete -> read_extended_results ()
                      | Protocol.BindComplete -> read_extended_results ()
                      | Protocol.RowDescription fields ->
                          column_info := fields;
                          read_extended_results ()
                      | Protocol.DataRow cols ->
                          let row =
                            if List.length !column_info = List.length cols then
                              List.map2
                                (fun (field : Protocol.field) value ->
                                  let decoded_value =
                                    decode_value field value
                                  in
                                  (field.name, decoded_value))
                                !column_info cols
                            else
                              List.mapi
                                (fun i v ->
                                  ("col_" ^ string_of_int i, Sqlx_driver.Value.string v))
                                cols
                          in
                          Collections.Queue.push result_set.rows row;
                          read_extended_results ()
                      | Protocol.CommandComplete tag ->
                          Log.debug ("Command complete: " ^ tag);
                          let parts = String.split_on_char ' ' tag in
                          (match List.rev parts with
                          | n :: _ -> (
                              match int_of_string_opt n with
                              | Some count -> result_set.rows_affected <- count
                              | None -> ())
                          | [] -> ());
                          read_extended_results ()
                      | Protocol.ReadyForQuery _status -> Ok result_set
                      | Protocol.ErrorResponse fields ->
                          let msg =
                            List.assoc_opt 'M' fields
                            |> Option.unwrap_or ~default:"Unknown error"
                          in
                          Error ("Query error: " ^ msg)
                      | Protocol.NoticeResponse fields ->
                          let msg =
                            List.assoc_opt 'M' fields
                            |> Option.unwrap_or ~default:""
                          in
                          Log.info ("PostgreSQL notice: " ^ msg);
                          read_extended_results ()
                      | Protocol.NoData -> read_extended_results ()
                      | Protocol.EmptyQueryResponse -> Ok result_set
                      | _ -> read_extended_results ())
                in
                read_extended_results ()
          else
            let query_msg = Protocol.Writer.query_message stmt.sql in

            match write_message stream query_msg with
            | Error e -> Error e
            | Ok () ->
                let result_set =
                  { rows = Collections.Queue.create (); rows_affected = 0 }
                in
                let column_info = ref [] in
                let rec read_query_results () =
                  match read_message stream with
                  | Error e -> Error e
                  | Ok (msg_type, length, body) -> (
                      let backend_msg =
                        Protocol.Reader.parse_backend_message msg_type length
                          body
                      in
                      match backend_msg with
                      | Protocol.RowDescription fields ->
                          column_info := fields;
                          read_query_results ()
                      | Protocol.DataRow cols ->
                          let row =
                            if List.length !column_info = List.length cols then
                              List.map2
                                (fun (field : Protocol.field) value ->
                                  let decoded_value =
                                    decode_value field value
                                  in
                                  (field.name, decoded_value))
                                !column_info cols
                            else
                              List.mapi
                                (fun i v ->
                                  ("col_" ^ string_of_int i, Sqlx_driver.Value.string v))
                                cols
                          in
                          Collections.Queue.push result_set.rows row;
                          read_query_results ()
                      | Protocol.CommandComplete tag ->
                          Log.debug ("Command complete: " ^ tag);
                          let parts = String.split_on_char ' ' tag in
                          (match List.rev parts with
                          | n :: _ -> (
                              match int_of_string_opt n with
                              | Some count -> result_set.rows_affected <- count
                              | None -> ())
                          | [] -> ());
                          read_query_results ()
                      | Protocol.ReadyForQuery _status -> Ok result_set
                      | Protocol.ErrorResponse fields ->
                          let msg =
                            List.assoc_opt 'M' fields
                            |> Option.unwrap_or ~default:"Unknown error"
                          in
                          Error ("Query error: " ^ msg)
                      | Protocol.NoticeResponse fields ->
                          let msg =
                            List.assoc_opt 'M' fields
                            |> Option.unwrap_or ~default:""
                          in
                          Log.info ("PostgreSQL notice: " ^ msg);
                          read_query_results ()
                      | _ ->
                          Error
                            ("Unexpected message during query: " ^
                               String.make 1 (Char.chr msg_type)))
                in
                read_query_results ())

  let fetch_row result_set = Collections.Queue.pop result_set.rows
  let rows_affected result_set = result_set.rows_affected

  let begin_transaction conn =
    if conn.closed then Error "Connection is closed"
    else (
      conn.transaction_status <- 'T';
      Ok ())

  let commit conn =
    if conn.closed then Error "Connection is closed"
    else if conn.transaction_status != 'T' then
      Error "No transaction in progress"
    else (
      conn.transaction_status <- 'I';
      Ok ())

  let rollback conn =
    if conn.closed then Error "Connection is closed"
    else if conn.transaction_status != 'T' then
      Error "No transaction in progress"
    else (
      conn.transaction_status <- 'I';
      Ok ())

  let set_isolation_level _conn = function
    | `Read_uncommitted -> Ok ()
    | `Read_committed -> Ok ()
    | `Repeatable_read -> Ok ()
    | `Serializable -> Ok ()
end
