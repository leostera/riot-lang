open Std

type message_type =
  | Startup
  | Query
  | Terminate
  | PasswordMessage
  | Parse
  | Bind
  | Execute
  | Describe
  | Close
  | Sync

type backend_message =
  | AuthenticationOk
  | AuthenticationCleartextPassword
  | AuthenticationMD5Password of bytes
  | BackendKeyData of { process_id : int; secret_key : int }
  | ParameterStatus of { name : string; value : string }
  | ReadyForQuery of char
  | RowDescription of field list
  | DataRow of string list
  | CommandComplete of string
  | ErrorResponse of (char * string) list
  | NoticeResponse of (char * string) list
  | ParseComplete
  | BindComplete
  | CloseComplete
  | NoData
  | ParameterDescription of int list
  | EmptyQueryResponse

and field = {
  name : string;
  table_oid : int;
  column_attr : int;
  type_oid : int;
  type_size : int;
  type_modifier : int;
  format_code : int;
}

module TypeOid = struct
  let bool = 16
  let bytea = 17
  let char = 18
  let int8 = 20
  let int2 = 21
  let int4 = 23
  let text = 25
  let oid = 26
  let json = 114
  let float4 = 700
  let float8 = 701
  let varchar = 1043
  let date = 1082
  let time = 1083
  let timestamp = 1114
  let timestamptz = 1184
  let interval = 1186
  let numeric = 1700
  let uuid = 2950
  let jsonb = 3802
end

module Writer = struct
  let write_int32 buf n =
    Buffer.add_char buf (Char.chr ((n lsr 24) land 0xFF));
    Buffer.add_char buf (Char.chr ((n lsr 16) land 0xFF));
    Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))

  let write_int16 buf n =
    Buffer.add_char buf (Char.chr ((n lsr 8) land 0xFF));
    Buffer.add_char buf (Char.chr (n land 0xFF))

  let write_string buf s =
    Buffer.add_string buf s;
    Buffer.add_char buf '\x00'

  let startup_message ~user ~database ~application_name =
    let buf = Buffer.create 256 in
    write_int32 buf 0;
    write_int32 buf 196608;
    write_string buf "user";
    write_string buf user;
    write_string buf "database";
    write_string buf database;
    (match application_name with
    | Some name ->
        write_string buf "application_name";
        write_string buf name
    | None -> ());
    Buffer.add_char buf '\x00';

    let content = Buffer.contents buf in
    let len = String.length content in
    let result = Buffer.create (len + 4) in
    write_int32 result len;
    Buffer.add_string result (String.sub content 4 (len - 4));
    Buffer.contents result

  let query_message sql =
    let buf = Buffer.create (String.length sql + 8) in
    Buffer.add_char buf 'Q';
    write_int32 buf (String.length sql + 5);
    write_string buf sql;
    Buffer.contents buf

  let parse_message ~statement_name ~query ~param_types =
    let buf = Buffer.create 256 in
    write_string buf statement_name;
    write_string buf query;
    write_int16 buf (List.length param_types);
    List.iter (fun oid -> write_int32 buf oid) param_types;
    let content = Buffer.contents buf in
    let length = String.length content + 4 in
    let result = Buffer.create (length + 1) in
    Buffer.add_char result 'P';
    write_int32 result length;
    Buffer.add_string result content;
    Buffer.contents result

  let bind_message ~portal_name ~statement_name ~params =
    let buf = Buffer.create 256 in
    write_string buf portal_name;
    write_string buf statement_name;
    write_int16 buf 0;
    write_int16 buf (List.length params);
    List.iter
      (fun param ->
        write_int32 buf (String.length param);
        Buffer.add_string buf param)
      params;
    write_int16 buf 0;
    let content = Buffer.contents buf in
    let length = String.length content + 4 in
    let result = Buffer.create (length + 1) in
    Buffer.add_char result 'B';
    write_int32 result length;
    Buffer.add_string result content;
    Buffer.contents result

  let execute_message ~portal_name ~max_rows =
    let buf = Buffer.create 64 in
    Buffer.add_char buf 'E';
    write_int32 buf (String.length portal_name + 1 + 4 + 4);
    write_string buf portal_name;
    write_int32 buf max_rows;
    Buffer.contents buf

  let describe_message ~what ~name =
    let buf = Buffer.create 64 in
    Buffer.add_char buf 'D';
    write_int32 buf (1 + String.length name + 1 + 4);
    Buffer.add_char buf what;
    write_string buf name;
    Buffer.contents buf

  let sync_message () =
    let buf = Buffer.create 5 in
    Buffer.add_char buf 'S';
    write_int32 buf 4;
    Buffer.contents buf

  let close_message ~what ~name =
    let buf = Buffer.create 64 in
    Buffer.add_char buf 'C';
    write_int32 buf (1 + String.length name + 1 + 4);
    Buffer.add_char buf what;
    write_string buf name;
    Buffer.contents buf

  let terminate_message () =
    let buf = Buffer.create 5 in
    Buffer.add_char buf 'X';
    write_int32 buf 4;
    Buffer.contents buf
end

module Reader = struct
  let parse_backend_message msg_type _length bytes =
    let reader = Binary_reader.create bytes in
    let msg_char = Char.chr msg_type in

    match msg_char with
    | 'R' -> (
        let auth_type =
          Binary_reader.read_int32 reader
          |> Option.expect
               ~msg:
                 "Protocol error: expected auth_type in Authentication message"
        in
        match auth_type with
        | 0 -> AuthenticationOk
        | 3 -> AuthenticationCleartextPassword
        | 5 ->
            let salt =
              Binary_reader.read_bytes reader 4
              |> Option.expect
                   ~msg:
                     "Protocol error: expected salt in \
                      AuthenticationMD5Password"
            in
            AuthenticationMD5Password salt
        | n -> panic (format "Unknown authentication type: %d" n))
    | 'K' ->
        let process_id =
          Binary_reader.read_int32 reader
          |> Option.expect
               ~msg:"Protocol error: expected process_id in BackendKeyData"
        in
        let secret_key =
          Binary_reader.read_int32 reader
          |> Option.expect
               ~msg:"Protocol error: expected secret_key in BackendKeyData"
        in
        BackendKeyData { process_id; secret_key }
    | 'S' ->
        let name =
          Binary_reader.read_string reader
          |> Option.expect
               ~msg:"Protocol error: expected name in ParameterStatus"
        in
        let value =
          Binary_reader.read_string reader
          |> Option.expect
               ~msg:"Protocol error: expected value in ParameterStatus"
        in
        ParameterStatus { name; value }
    | 'Z' ->
        let status =
          Binary_reader.read_byte reader
          |> Option.expect
               ~msg:"Protocol error: expected status in ReadyForQuery"
        in
        ReadyForQuery (Char.chr status)
    | 'T' ->
        let field_count =
          Binary_reader.read_int16 reader
          |> Option.expect
               ~msg:"Protocol error: expected field_count in RowDescription"
        in
        let rec read_fields n acc =
          if n = 0 then List.rev acc
          else
            let name =
              Binary_reader.read_string reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected field name (field %d)"
                        (field_count - n + 1))
            in
            let table_oid =
              Binary_reader.read_int32 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected table_oid (field %s)"
                        name)
            in
            let column_attr =
              Binary_reader.read_int16 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected column_attr (field %s)"
                        name)
            in
            let type_oid =
              Binary_reader.read_int32 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected type_oid (field %s)" name)
            in
            let type_size =
              Binary_reader.read_int16 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected type_size (field %s)"
                        name)
            in
            let type_modifier =
              Binary_reader.read_int32 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected type_modifier (field %s)"
                        name)
            in
            let format_code =
              Binary_reader.read_int16 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected format_code (field %s)"
                        name)
            in
            let field =
              {
                name;
                table_oid;
                column_attr;
                type_size;
                type_oid;
                type_modifier;
                format_code;
              }
            in
            read_fields (n - 1) (field :: acc)
        in
        RowDescription (read_fields field_count [])
    | 'D' ->
        let col_count =
          Binary_reader.read_int16 reader
          |> Option.expect
               ~msg:"Protocol error: expected column_count in DataRow"
        in
        let rec read_columns n acc =
          if n = 0 then List.rev acc
          else
            let col_len =
              Binary_reader.read_int32 reader
              |> Option.expect
                   ~msg:
                     (format "Protocol error: expected column_length (col %d)"
                        (col_count - n + 1))
            in
            if col_len = -1 then read_columns (n - 1) ("" :: acc)
            else
              let value =
                Binary_reader.read_cstring reader col_len
                |> Option.expect
                     ~msg:
                       (format "Protocol error: expected column_value (col %d)"
                          (col_count - n + 1))
              in
              read_columns (n - 1) (value :: acc)
        in
        DataRow (read_columns col_count [])
    | 'C' ->
        let tag =
          Binary_reader.read_string reader
          |> Option.expect
               ~msg:"Protocol error: expected tag in CommandComplete"
        in
        CommandComplete tag
    | 'E' | 'N' ->
        let rec read_fields acc =
          if Binary_reader.is_eof reader then List.rev acc
          else
            match Binary_reader.read_byte reader with
            | None -> List.rev acc
            | Some 0 -> List.rev acc
            | Some field_type -> (
                match Binary_reader.read_string reader with
                | None -> List.rev acc
                | Some value -> read_fields ((Char.chr field_type, value) :: acc)
                )
        in
        let fields = read_fields [] in
        if msg_char = 'E' then ErrorResponse fields else NoticeResponse fields
    | '1' -> ParseComplete
    | '2' -> BindComplete
    | '3' -> CloseComplete
    | 'n' -> NoData
    | 'I' -> EmptyQueryResponse
    | 't' ->
        let param_count =
          Binary_reader.read_int16 reader
          |> Option.expect
               ~msg:
                 "Protocol error: expected param_count in ParameterDescription"
        in
        let rec read_oids n acc =
          if n = 0 then List.rev acc
          else
            let oid =
              Binary_reader.read_int32 reader
              |> Option.expect
                   ~msg:"Protocol error: expected OID in ParameterDescription"
            in
            read_oids (n - 1) (oid :: acc)
        in
        ParameterDescription (read_oids param_count [])
    | c -> panic (format "Unknown message type: '%c' (0x%02x)" c msg_type)
end
