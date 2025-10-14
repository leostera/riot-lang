open Std

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
        let hostname = Net.Uri.host uri |> Option.unwrap_or ~default:"localhost" in
        let port = Net.Uri.port uri |> Option.unwrap_or ~default:5432 in
        
        let host = 
          match Net.Addr.of_host_and_port ~host:hostname ~port with
          | Ok (`Tcp (resolved_ip, _)) -> 
              if resolved_ip = "::1" then "127.0.0.1"
              else resolved_ip
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
    prepared_statements : (string, statement) Hashtbl.t;
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
    | Error `Connection_refused -> Error "Write error: Connection refused"
    | Error `Closed -> Error "Write error: Connection closed"
    | Error (`System_error msg) -> Error (format "Write error: %s" msg)
    | Ok _n -> Ok ()

  let read_exact stream buf len =
    let rec loop offset remaining =
      if remaining = 0 then Ok ()
      else
        match Net.TcpStream.read stream buf ~pos:offset ~len:remaining () with
        | Error e -> Error e
        | Ok 0 -> Error `Closed
        | Ok n -> loop (offset + n) (remaining - n)
    in
    loop 0 len

  let read_message stream =
    let header = Bytes.create 5 in
    match read_exact stream header 5 with
    | Error `Connection_refused -> Error "Read error: Connection refused"
    | Error `Closed -> Error "Read error: Connection closed"
    | Error (`System_error msg) -> Error (format "Read error: %s" msg)
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
          | Error `Connection_refused ->
              Error "Read body error: Connection refused"
          | Error `Closed -> Error "Read body error: Connection closed"
          | Error (`System_error msg) -> Error (format "Read body error: %s" msg)
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
                  Log.debug "PostgreSQL parameter: %s = %s" name value;
                  read_until_ready ()
              | Protocol.BackendKeyData { process_id; secret_key } ->
                  Log.debug "Backend key data: pid=%d secret=%d" process_id
                    secret_key;
                  read_until_ready ()
              | Protocol.ReadyForQuery status ->
                  Log.debug "Ready for query, status: %c" status;
                  Ok ()
              | Protocol.ErrorResponse fields ->
                  let msg =
                    List.assoc_opt 'M' fields
                    |> Option.unwrap_or ~default:"Unknown error"
                  in
                  Error (format "PostgreSQL error: %s" msg)
              | Protocol.NoticeResponse fields ->
                  let msg =
                    List.assoc_opt 'M' fields |> Option.unwrap_or ~default:""
                  in
                  Log.info "PostgreSQL notice: %s" msg;
                  read_until_ready ()
              | _ ->
                  Error
                    (format "Unexpected message during handshake: %c"
                       (Char.chr msg_type)))
        in
        read_until_ready ()

  let connect (cfg : Config.t) =
    let id = format "pg_%d" (Random.int 1000000) in

    match Net.Addr.of_host_and_port ~host:cfg.host ~port:cfg.port with
    | Error (`System_error msg) ->
        Error (format "Failed to resolve host %s: %s" cfg.host msg)
    | Ok addr -> (
        match Net.TcpStream.connect addr with
        | Error `Connection_refused ->
            Error (format "Connection refused to %s:%d" cfg.host cfg.port)
        | Error `Closed -> Error "Connection closed unexpectedly"
        | Error (`System_error msg) -> Error (format "System error: %s" msg)
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
                    prepared_statements = Hashtbl.create 16;
                  }))

  let close conn =
    conn.closed <- true;
    match conn.stream with
    | Some stream -> Net.TcpStream.close stream
    | None -> ()

  let ping conn = not conn.closed

  let prepare conn sql =
    if conn.closed then Error "Connection is closed"
    else
      let name = format "stmt_%d" (Random.int 1000000) in
      let stmt = { name; sql; conn } in
      Hashtbl.add conn.prepared_statements name stmt;
      Ok stmt

  let execute stmt _params =
    if stmt.conn.closed then Error "Connection is closed"
    else
      match stmt.conn.stream with
      | None -> Error "No active stream"
      | Some stream -> (
          let query_msg = Protocol.Writer.query_message stmt.sql in

          match write_message stream query_msg with
          | Error e -> Error e
          | Ok () ->
              let result_set =
                { rows = Collections.Queue.create (); rows_affected = 0 }
              in
              let column_names = ref [] in
              let rec read_query_results () =
                match read_message stream with
                | Error e -> Error e
                | Ok (msg_type, length, body) -> (
                    let backend_msg =
                      Protocol.Reader.parse_backend_message msg_type length body
                    in
                    match backend_msg with
                    | Protocol.RowDescription fields ->
                        column_names :=
                          List.map (fun f -> f.Protocol.name) fields;
                        read_query_results ()
                    | Protocol.DataRow cols ->
                        let row =
                          if List.length !column_names = List.length cols then
                            List.map2
                              (fun name value ->
                                (name, Sqlx_driver.Value.string value))
                              !column_names cols
                          else
                            List.mapi
                              (fun i v ->
                                (format "col_%d" i, Sqlx_driver.Value.string v))
                              cols
                        in
                        Collections.Queue.enqueue result_set.rows row;
                        read_query_results ()
                    | Protocol.CommandComplete tag ->
                        Log.debug "Command complete: %s" tag;
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
                        Error (format "Query error: %s" msg)
                    | Protocol.NoticeResponse fields ->
                        let msg =
                          List.assoc_opt 'M' fields
                          |> Option.unwrap_or ~default:""
                        in
                        Log.info "PostgreSQL notice: %s" msg;
                        read_query_results ()
                    | _ ->
                        Error
                          (format "Unexpected message during query: %c"
                             (Char.chr msg_type)))
              in
              read_query_results ())

  let fetch_row result_set = Collections.Queue.dequeue result_set.rows
  let rows_affected result_set = result_set.rows_affected

  let begin_transaction conn =
    if conn.closed then Error "Connection is closed"
    else (
      conn.transaction_status <- 'T';
      Ok ())

  let commit conn =
    if conn.closed then Error "Connection is closed"
    else if conn.transaction_status <> 'T' then
      Error "No transaction in progress"
    else (
      conn.transaction_status <- 'I';
      Ok ())

  let rollback conn =
    if conn.closed then Error "Connection is closed"
    else if conn.transaction_status <> 'T' then
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
