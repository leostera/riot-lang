open Std
open Result.Syntax

module Value = Sqlx_driver.Value
module Buffer = Std.StringBuilder
module Bytes = Std.IO.Bytes

type parse_error =
  | Truncated of string
  | InvalidPacketLength of int
  | InvalidPacketSequence of { expected: int; actual: int }
  | InvalidLengthEncodedInteger of string
  | InvalidCString of string
  | UnknownPacket of int
  | UnsupportedPacket of string
  | InvalidPayload of string

let parse_error_to_string = fun error ->
  match error with
  | Truncated context -> "truncated MySQL packet while reading " ^ context
  | InvalidPacketLength length -> "invalid MySQL packet length: " ^ Int.to_string length
  | InvalidPacketSequence { expected; actual } ->
      "invalid MySQL packet sequence: expected "
      ^ Int.to_string expected
      ^ ", got "
      ^ Int.to_string actual
  | InvalidLengthEncodedInteger context ->
      "invalid MySQL length-encoded integer while reading " ^ context
  | InvalidCString context -> "invalid MySQL null-terminated string while reading " ^ context
  | UnknownPacket marker -> "unknown MySQL packet marker: " ^ Int.to_string marker
  | UnsupportedPacket message -> "unsupported MySQL packet: " ^ message
  | InvalidPayload message -> "invalid MySQL payload: " ^ message

module Capability = struct
  type t = int

  let long_password = 0x0000_0001

  let found_rows = 0x0000_0002

  let long_flag = 0x0000_0004

  let connect_with_db = 0x0000_0008

  let protocol_41 = 0x0000_0200

  let ssl = 0x0000_0800

  let transactions = 0x0000_2000

  let secure_connection = 0x0000_8000

  let multi_results = 0x0002_0000

  let ps_multi_results = 0x0004_0000

  let plugin_auth = 0x0008_0000

  let connect_attrs = 0x0010_0000

  let plugin_auth_lenenc_client_data = 0x0020_0000

  let session_track = 0x0080_0000

  let deprecate_eof = 0x0100_0000

  let has = fun flags capability -> flags land capability = capability

  let ssl_capability = ssl

  let default_client = fun ?(database = false) ?(ssl = false) () ->
    let flags =
      long_password
      lor found_rows
      lor long_flag
      lor protocol_41
      lor transactions
      lor secure_connection
      lor plugin_auth
      lor plugin_auth_lenenc_client_data
      lor session_track
    in
    let flags =
      if database then
        flags lor connect_with_db
      else
        flags
    in
    if ssl then
      flags lor ssl_capability
    else
      flags
end

let byte_at = fun text index -> Char.code (String.get_unchecked text ~at:index)

let add_u8 = fun buffer value -> Buffer.add_char buffer (Char.from_int_unchecked (value land 0xff))

let add_int16_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8)

let add_int24_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8);
  add_u8 buffer (value lsr 16)

let add_int32_le = fun buffer value ->
  add_u8 buffer value;
  add_u8 buffer (value lsr 8);
  add_u8 buffer (value lsr 16);
  add_u8 buffer (value lsr 24)

let add_int64_le = fun buffer value ->
  for shift = 0 to 7 do
    let byte =
      Int64.shift_right_logical value (shift * 8)
      |> Int64.logand 0xffL
      |> Int64.to_int
    in
    add_u8 buffer byte
  done

let add_cstring = fun buffer value ->
  Buffer.add_string buffer value;
  add_u8 buffer 0

let int64_of_u8 = fun value -> Int64.from_int (value land 0xff)

let xor_strings = fun left right ->
  let len = String.length left in
  let bytes = Bytes.create ~size:len in
  for index = 0 to len - 1 do
    let l = byte_at left index in
    let r = byte_at right index in
    Bytes.set_unchecked bytes ~at:index ~char:(Char.from_int_unchecked (l lxor r))
  done;
  Bytes.to_string bytes

let strip_at_nul = fun value ->
  match String.index_of value ~char:'\x00' with
  | Some index -> String.sub value ~offset:0 ~len:index
  | None -> value

let sha1_bytes = fun value ->
  Crypto.Sha1.hash_string value
  |> Crypto.Digest.bytes
  |> Bytes.to_string

let sha256_bytes = fun value ->
  Crypto.Sha256.hash_string value
  |> Crypto.Digest.bytes
  |> Bytes.to_string

module Packet = struct
  type t = { sequence: int; payload: string }

  let max_payload_length = 0x00ff_ffff

  let make = fun ~sequence ~payload -> { sequence = sequence land 0xff; payload }

  let frame = fun sequence payload ->
    let length = String.length payload in
    let buffer = Buffer.create ~size:(length + 4) in
    add_int24_le buffer length;
    add_u8 buffer (sequence land 0xff);
    Buffer.add_string buffer payload;
    Buffer.contents buffer

  let encode = fun packet ->
    let total = String.length packet.payload in
    let rec loop offset sequence acc =
      if total - offset > max_payload_length then
        let chunk = String.sub packet.payload ~offset ~len:max_payload_length in
        loop (offset + max_payload_length) ((sequence + 1) land 0xff) (frame sequence chunk :: acc)
      else
        let len = total - offset in
        let chunk = String.sub packet.payload ~offset ~len in
        let acc = frame sequence chunk :: acc in
        let acc =
          if len = max_payload_length then
            frame ((sequence + 1) land 0xff) "" :: acc
          else
            acc
        in
        List.rev acc
    in
    loop 0 packet.sequence []

  let decode_one = fun frame ->
    if String.length frame < 4 then
      Error (Truncated "packet header")
    else
      let length = byte_at frame 0 lor (byte_at frame 1 lsl 8) lor (byte_at frame 2 lsl 16) in
      let sequence = byte_at frame 3 in
      if length < 0 || length > max_payload_length then
        Error (InvalidPacketLength length)
      else if String.length frame < length + 4 then
        Error (Truncated "packet payload")
      else
        Ok { sequence; payload = String.sub frame ~offset:4 ~len:length }
end

module ColumnType = struct
  type t =
    | Tiny
    | Short
    | Long
    | Float
    | Double
    | Null
    | Timestamp
    | LongLong
    | Int24
    | Date
    | Time
    | DateTime
    | Year
    | VarChar
    | Json
    | NewDecimal
    | Enum
    | Set
    | TinyBlob
    | MediumBlob
    | LongBlob
    | Blob
    | VarString
    | String
    | Geometry
    | Unknown of int

  let from_int = fun value ->
    match value with
    | 0x00 -> NewDecimal
    | 0x01 -> Tiny
    | 0x02 -> Short
    | 0x03 -> Long
    | 0x04 -> Float
    | 0x05 -> Double
    | 0x06 -> Null
    | 0x07 -> Timestamp
    | 0x08 -> LongLong
    | 0x09 -> Int24
    | 0x0a -> Date
    | 0x0b -> Time
    | 0x0c -> DateTime
    | 0x0d -> Year
    | 0x0f -> VarChar
    | 0xf5 -> Json
    | 0xf7 -> Enum
    | 0xf8 -> Set
    | 0xf9 -> TinyBlob
    | 0xfa -> MediumBlob
    | 0xfb -> LongBlob
    | 0xfc -> Blob
    | 0xfd -> VarString
    | 0xfe -> String
    | 0xff -> Geometry
    | value -> Unknown value

  let to_int = fun value ->
    match value with
    | NewDecimal -> 0x00
    | Tiny -> 0x01
    | Short -> 0x02
    | Long -> 0x03
    | Float -> 0x04
    | Double -> 0x05
    | Null -> 0x06
    | Timestamp -> 0x07
    | LongLong -> 0x08
    | Int24 -> 0x09
    | Date -> 0x0a
    | Time -> 0x0b
    | DateTime -> 0x0c
    | Year -> 0x0d
    | VarChar -> 0x0f
    | Json -> 0xf5
    | Enum -> 0xf7
    | Set -> 0xf8
    | TinyBlob -> 0xf9
    | MediumBlob -> 0xfa
    | LongBlob -> 0xfb
    | Blob -> 0xfc
    | VarString -> 0xfd
    | String -> 0xfe
    | Geometry -> 0xff
    | Unknown value -> value

  let to_string = fun value ->
    match value with
    | Tiny -> "TINY"
    | Short -> "SHORT"
    | Long -> "LONG"
    | Float -> "FLOAT"
    | Double -> "DOUBLE"
    | Null -> "NULL"
    | Timestamp -> "TIMESTAMP"
    | LongLong -> "LONGLONG"
    | Int24 -> "INT24"
    | Date -> "DATE"
    | Time -> "TIME"
    | DateTime -> "DATETIME"
    | Year -> "YEAR"
    | VarChar -> "VARCHAR"
    | Json -> "JSON"
    | NewDecimal -> "NEWDECIMAL"
    | Enum -> "ENUM"
    | Set -> "SET"
    | TinyBlob -> "TINYBLOB"
    | MediumBlob -> "MEDIUMBLOB"
    | LongBlob -> "LONGBLOB"
    | Blob -> "BLOB"
    | VarString -> "VAR_STRING"
    | String -> "STRING"
    | Geometry -> "GEOMETRY"
    | Unknown value -> "UNKNOWN(" ^ Int.to_string value ^ ")"
end

module ServerStatus = struct
  type t = { flags: int; in_transaction: bool; autocommit: bool; more_results: bool }

  let from_int = fun flags ->
    {
      flags;
      in_transaction = flags land 0x0001 != 0;
      autocommit = flags land 0x0002 != 0;
      more_results = flags land 0x0008 != 0;
    }
end

module Error = struct
  type t = {
    code: int;
    sql_state: string option;
    message: string;
  }

  let to_string = fun error ->
    let prefix =
      match error.sql_state with
      | Some sql_state -> "MySQL error " ^ Int.to_string error.code ^ " (" ^ sql_state ^ ")"
      | None -> "MySQL error " ^ Int.to_string error.code
    in
    prefix ^ ": " ^ error.message

  let to_json = fun error ->
    let fields = [
      ("type", Data.Json.string "mysql_error");
      ("code", Data.Json.int error.code);
      ("message", Data.Json.string error.message);
    ]
    in
    let fields =
      match error.sql_state with
      | Some state -> fields @ [ ("sql_state", Data.Json.string state) ]
      | None -> fields
    in
    Data.Json.obj fields
end

type ok_packet = {
  affected_rows: int64;
  last_insert_id: int64;
  status: ServerStatus.t;
  warnings: int;
  info: string;
}

type handshake = {
  protocol_version: int;
  server_version: string;
  connection_id: int;
  auth_plugin_data: string;
  capability_flags: int;
  character_set: int;
  status_flags: int;
  auth_plugin_name: string option;
}

type column_definition = {
  schema: string;
  table: string;
  org_table: string;
  name: string;
  org_name: string;
  character_set: int;
  column_length: int;
  column_type: ColumnType.t;
  flags: int;
  decimals: int;
}

type prepare_ok = { statement_id: int; num_columns: int; num_params: int; warnings: int }

type row_value =
  | Null
  | Value of string

module Auth = struct
  let mysql_native_password = fun ~password ~seed ->
    if String.is_empty password then
      ""
    else
      let stage1 = sha1_bytes password in
      let stage2 = sha1_bytes stage1 in
      let stage3 = sha1_bytes (seed ^ stage2) in
      xor_strings stage1 stage3

  let caching_sha2_password = fun ~password ~seed ->
    if String.is_empty password then
      ""
    else
      let stage1 = sha256_bytes password in
      let stage2 = sha256_bytes stage1 in
      let stage3 = sha256_bytes (stage2 ^ seed) in
      xor_strings stage1 stage3
end

module Reader = struct
  type cursor = {
    data: string;
    mutable pos: int;
  }

  let cursor = fun data -> { data; pos = 0 }

  let remaining = fun cursor -> String.length cursor.data - cursor.pos

  let require = fun cursor len context ->
    if len < 0 then
      Error (InvalidPayload ("negative length while reading " ^ context))
    else if remaining cursor < len then
      Error (Truncated context)
    else
      Ok ()

  let read_u8 = fun cursor context ->
    match require cursor 1 context with
    | Error _ as error -> error
    | Ok () ->
        let value = byte_at cursor.data cursor.pos in
        cursor.pos <- cursor.pos + 1;
        Ok value

  let read_bytes = fun cursor len context ->
    match require cursor len context with
    | Error _ as error -> error
    | Ok () ->
        let value = String.sub cursor.data ~offset:cursor.pos ~len in
        cursor.pos <- cursor.pos + len;
        Ok value

  let skip = fun cursor len context ->
    match require cursor len context with
    | Error _ as error -> error
    | Ok () ->
        cursor.pos <- cursor.pos + len;
        Ok ()

  let read_int16_le = fun cursor context ->
    match read_bytes cursor 2 context with
    | Error _ as error -> error
    | Ok bytes -> Ok (byte_at bytes 0 lor (byte_at bytes 1 lsl 8))

  let read_int24_le = fun cursor context ->
    match read_bytes cursor 3 context with
    | Error _ as error -> error
    | Ok bytes -> Ok (byte_at bytes 0 lor (byte_at bytes 1 lsl 8) lor (byte_at bytes 2 lsl 16))

  let read_int32_le = fun cursor context ->
    match read_bytes cursor 4 context with
    | Error _ as error -> error
    | Ok bytes ->
        Ok (byte_at bytes 0
        lor (byte_at bytes 1 lsl 8)
        lor (byte_at bytes 2 lsl 16)
        lor (byte_at bytes 3 lsl 24))

  let read_int64_le = fun cursor context ->
    match read_bytes cursor 8 context with
    | Error _ as error -> error
    | Ok bytes ->
        let value = ref 0L in
        for index = 0 to 7 do
          value := Int64.logor
            !value
            (Int64.shift_left (int64_of_u8 (byte_at bytes index)) (index * 8))
        done;
        Ok !value

  let read_cstring = fun cursor context ->
    let rec find index =
      if index >= String.length cursor.data then
        None
      else if byte_at cursor.data index = 0 then
        Some index
      else
        find (index + 1)
    in
    match find cursor.pos with
    | None -> Error (InvalidCString context)
    | Some terminator ->
        let len = terminator - cursor.pos in
        let value = String.sub cursor.data ~offset:cursor.pos ~len in
        cursor.pos <- terminator + 1;
        Ok value

  let read_lenenc_int = fun cursor context ->
    match read_u8 cursor context with
    | Error _ as error -> error
    | Ok first ->
        if first < 0xfb then
          Ok (Int64.from_int first)
        else
          match first with
          | 0xfc -> (
              match read_int16_le cursor context with
              | Ok value -> Ok (Int64.from_int value)
              | Error _ as error -> error
            )
          | 0xfd -> (
              match read_int24_le cursor context with
              | Ok value -> Ok (Int64.from_int value)
              | Error _ as error -> error
            )
          | 0xfe -> read_int64_le cursor context
          | _ -> Error (InvalidLengthEncodedInteger context)

  let read_lenenc_string = fun cursor context ->
    match read_lenenc_int cursor context with
    | Error _ as error -> error
    | Ok length ->
        if
          Int64.compare length 0L = Order.LT
          || Int64.compare length (Int64.from_int Int.max_int) = Order.GT
        then
          Error (InvalidLengthEncodedInteger context)
        else
          let len = Int64.to_int length in
          read_bytes cursor len context

  let read_lenenc_string_nullable = fun cursor context ->
    match require cursor 1 context with
    | Error _ as error -> error
    | Ok () ->
        if byte_at cursor.data cursor.pos = 0xfb then (
          cursor.pos <- cursor.pos + 1;
          Ok None
        ) else
          match read_lenenc_string cursor context with
          | Ok value -> Ok (Some value)
          | Error _ as error -> error

  let parse_error_packet = fun payload ->
    let cursor = cursor payload in
    match read_u8 cursor "error marker" with
    | Error _ as error -> error
    | Ok marker when marker != 0xff -> Error (UnknownPacket marker)
    | Ok _ -> (
        match read_int16_le cursor "error code" with
        | Error _ as error -> error
        | Ok code ->
            let (sql_state, message_offset) =
              if remaining cursor >= 6 && byte_at cursor.data cursor.pos = Char.code '#' then (
                cursor.pos <- cursor.pos + 1;
                let state = String.sub cursor.data ~offset:cursor.pos ~len:5 in
                (Some state, cursor.pos + 5)
              ) else
                (None, cursor.pos)
            in
            cursor.pos <- message_offset;
            Ok {
              Error.code;
              sql_state;
              message = String.sub cursor.data ~offset:cursor.pos ~len:(remaining cursor);
            }
      )

  let parse_ok_packet = fun payload ->
    let cursor = cursor payload in
    match read_u8 cursor "ok marker" with
    | Error _ as error -> error
    | Ok marker when marker != 0x00 && marker != 0xfe -> Error (UnknownPacket marker)
    | Ok _ -> (
        match read_lenenc_int cursor "affected rows" with
        | Error _ as error -> error
        | Ok affected_rows -> (
            match read_lenenc_int cursor "last insert id" with
            | Error _ as error -> error
            | Ok last_insert_id -> (
                match read_int16_le cursor "server status" with
                | Error _ as error -> error
                | Ok status_flags -> (
                    match read_int16_le cursor "warnings" with
                    | Error _ as error -> error
                    | Ok warnings ->
                        Ok {
                          affected_rows;
                          last_insert_id;
                          status = ServerStatus.from_int status_flags;
                          warnings;
                          info = String.sub cursor.data ~offset:cursor.pos ~len:(remaining cursor);
                        }
                  )
              )
          )
      )

  let parse_handshake = fun payload ->
    let cursor = cursor payload in
    let* protocol_version = read_u8 cursor "protocol version" in
    let* server_version = read_cstring cursor "server version" in
    let* connection_id = read_int32_le cursor "connection id" in
    let* auth_part_1 = read_bytes cursor 8 "auth plugin data part 1" in
    let* () = skip cursor 1 "filler" in
    let* lower_caps = read_int16_le cursor "lower capability flags" in
    if remaining cursor = 0 then
      Ok {
        protocol_version;
        server_version;
        connection_id;
        auth_plugin_data = auth_part_1;
        capability_flags = lower_caps;
        character_set = 0;
        status_flags = 0;
        auth_plugin_name = None;
      }
    else
      let* character_set = read_u8 cursor "character set" in
      let* status_flags = read_int16_le cursor "status flags" in
      let* upper_caps = read_int16_le cursor "upper capability flags" in
      let* auth_len = read_u8 cursor "auth plugin data length" in
      let* () = skip cursor 10 "reserved bytes" in
      let capability_flags = lower_caps lor (upper_caps lsl 16) in
      let part_2_len =
        if Capability.has capability_flags Capability.secure_connection then
          max 13 (auth_len - 8)
        else
          0
      in
      let actual_part_2_len = min (remaining cursor) part_2_len in
      let* auth_part_2 = read_bytes cursor actual_part_2_len "auth plugin data part 2" in
      let auth_plugin_name =
        if Capability.has capability_flags Capability.plugin_auth && remaining cursor > 0 then
          match read_cstring cursor "auth plugin name" with
          | Ok "" -> None
          | Ok name -> Some name
          | Error _ -> None
        else
          None
      in
      Ok {
        protocol_version;
        server_version;
        connection_id;
        auth_plugin_data = auth_part_1 ^ strip_at_nul auth_part_2;
        capability_flags;
        character_set;
        status_flags;
        auth_plugin_name;
      }

  let parse_column_definition = fun payload ->
    let cursor = cursor payload in
    match read_lenenc_string cursor "catalog" with
    | Error _ as error -> error
    | Ok _catalog -> (
        match read_lenenc_string cursor "schema" with
        | Error _ as error -> error
        | Ok schema -> (
            match read_lenenc_string cursor "table" with
            | Error _ as error -> error
            | Ok table -> (
                match read_lenenc_string cursor "original table" with
                | Error _ as error -> error
                | Ok org_table -> (
                    match read_lenenc_string cursor "name" with
                    | Error _ as error -> error
                    | Ok name -> (
                        match read_lenenc_string cursor "original name" with
                        | Error _ as error -> error
                        | Ok org_name -> (
                            match read_u8 cursor "fixed fields length" with
                            | Error _ as error -> error
                            | Ok _ -> (
                                match read_int16_le cursor "character set" with
                                | Error _ as error -> error
                                | Ok character_set -> (
                                    match read_int32_le cursor "column length" with
                                    | Error _ as error -> error
                                    | Ok column_length -> (
                                        match read_u8 cursor "column type" with
                                        | Error _ as error -> error
                                        | Ok column_type -> (
                                            match read_int16_le cursor "flags" with
                                            | Error _ as error -> error
                                            | Ok flags -> (
                                                match read_u8 cursor "decimals" with
                                                | Error _ as error -> error
                                                | Ok decimals ->
                                                    Ok {
                                                      schema;
                                                      table;
                                                      org_table;
                                                      name;
                                                      org_name;
                                                      character_set;
                                                      column_length;
                                                      column_type = ColumnType.from_int column_type;
                                                      flags;
                                                      decimals;
                                                    }
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )

  let parse_prepare_ok = fun payload ->
    let cursor = cursor payload in
    match read_u8 cursor "prepare ok marker" with
    | Error _ as error -> error
    | Ok marker when marker != 0x00 -> Error (UnknownPacket marker)
    | Ok _ -> (
        match read_int32_le cursor "statement id" with
        | Error _ as error -> error
        | Ok statement_id -> (
            match read_int16_le cursor "num columns" with
            | Error _ as error -> error
            | Ok num_columns -> (
                match read_int16_le cursor "num params" with
                | Error _ as error -> error
                | Ok num_params -> (
                    match skip cursor 1 "prepare filler" with
                    | Error _ as error -> error
                    | Ok () -> (
                        match read_int16_le cursor "warnings" with
                        | Error _ as error -> error
                        | Ok warnings ->
                            Ok {
                              statement_id;
                              num_columns;
                              num_params;
                              warnings;
                            }
                      )
                  )
              )
          )
      )

  let pad = fun value width ->
    let text = Int.to_string value in
    String.make ~len:(max 0 (width - String.length text)) ~char:'0' ^ text

  let parse_date_text = fun text ->
    match String.split_on_char '-' text with
    | [ year; month; day ] -> (
        match (Int.parse year, Int.parse month, Int.parse day) with
        | (Some y, Some m, Some d) when y > 0 && m > 0 && d > 0 -> Some (y, m, d)
        | _ -> None
      )
    | _ -> None

  let parse_time_text = fun text ->
    match String.split_on_char ':' text with
    | [ hour; minute; seconds ] -> (
        let (sec_text, micros) =
          match String.split_on_char '.' seconds with
          | [ sec; frac ] ->
              let padded =
                if String.length frac >= 6 then
                  String.sub frac ~offset:0 ~len:6
                else
                  frac ^ String.make ~len:(6 - String.length frac) ~char:'0'
              in
              (sec, Int.parse padded
              |> Option.unwrap_or ~default:0)
          | [ sec ] -> (sec, 0)
          | _ -> (seconds, 0)
        in
        match (Int.parse hour, Int.parse minute, Int.parse sec_text) with
        | (Some h, Some m, Some s) -> Some (h, m, s, micros)
        | _ -> None
      )
    | _ -> None

  let parse_datetime_text = fun text ->
    let split =
      match String.index_of text ~char:' ' with
      | Some index -> Some index
      | None -> String.index_of text ~char:'T'
    in
    match split with
    | None -> None
    | Some index ->
        let date_text = String.sub text ~offset:0 ~len:index in
        let time_text = String.sub text ~offset:(index + 1) ~len:(String.length text - index - 1) in
        match (parse_date_text date_text, parse_time_text time_text) with
        | (Some (year, month, day), Some (hour, minute, second, microsecond)) ->
            Some (
              DateTime.from_naive
                DateTime.{
                  year;
                  month;
                  day;
                  hour;
                  minute;
                  second;
                  microsecond;
                }
                ~tz:DateTime.Tz.Etc_UTC
            )
        | _ -> None

  let decode_text_value = fun column value ->
    match column.column_type with
    | ColumnType.Tiny
    | ColumnType.Short
    | ColumnType.Long
    | ColumnType.Int24
    | ColumnType.Year -> (
        match Int.parse value with
        | Some value -> Value.int value
        | None -> Value.string value
      )
    | ColumnType.LongLong -> (
        match Int64.from_string_opt value with
        | Some value -> Value.int64 value
        | None -> Value.string value
      )
    | ColumnType.Float
    | ColumnType.Double -> (
        match Float.parse value with
        | Some value -> Value.float value
        | None -> Value.string value
      )
    | ColumnType.NewDecimal -> Value.numeric value
    | ColumnType.Json -> Value.json value
    | ColumnType.Date -> (
        match parse_date_text value with
        | Some (year, month, day) -> Value.date year month day
        | None -> Value.string value
      )
    | ColumnType.Time -> (
        match parse_time_text value with
        | Some (hour, minute, second, microsecond) -> Value.time hour minute second microsecond
        | None -> Value.string value
      )
    | ColumnType.DateTime
    | ColumnType.Timestamp -> (
        match parse_datetime_text value with
        | Some value -> Value.timestamp value
        | None -> Value.string value
      )
    | ColumnType.TinyBlob
    | ColumnType.MediumBlob
    | ColumnType.LongBlob
    | ColumnType.Blob
    | ColumnType.Geometry -> Value.bytes (Bytes.from_string value)
    | ColumnType.Null -> Value.null
    | ColumnType.VarChar
    | ColumnType.Enum
    | ColumnType.Set
    | ColumnType.VarString
    | ColumnType.String
    | ColumnType.Unknown _ -> Value.string value

  let parse_text_row = fun columns payload ->
    let cursor = cursor payload in
    let rec loop columns acc =
      match columns with
      | [] ->
          if remaining cursor = 0 then
            Ok (List.rev acc)
          else
            Error (InvalidPayload "text row has trailing bytes")
      | column :: rest -> (
          match read_lenenc_string_nullable cursor ("text row column " ^ column.name) with
          | Error _ as error -> error
          | Ok None -> loop rest ((column.name, Value.null) :: acc)
          | Ok (Some value) -> loop rest ((column.name, decode_text_value column value) :: acc)
        )
    in
    loop columns []

  let decode_binary_datetime = fun column_type cursor ->
    match read_u8 cursor "binary date length" with
    | Error _ as error -> error
    | Ok 0 -> Ok (Value.string "0000-00-00")
    | Ok length when length = 4 || length = 7 || length = 11 -> (
        match read_int16_le cursor "binary date year" with
        | Error _ as error -> error
        | Ok year -> (
            match read_u8 cursor "binary date month" with
            | Error _ as error -> error
            | Ok month -> (
                match read_u8 cursor "binary date day" with
                | Error _ as error -> error
                | Ok day ->
                    if length = 4 then
                      Ok (Value.date year month day)
                    else
                      match read_u8 cursor "binary datetime hour" with
                      | Error _ as error -> error
                      | Ok hour -> (
                          match read_u8 cursor "binary datetime minute" with
                          | Error _ as error -> error
                          | Ok minute -> (
                              match read_u8 cursor "binary datetime second" with
                              | Error _ as error -> error
                              | Ok second -> (
                                  let read_micros =
                                    if length = 11 then
                                      read_int32_le cursor "binary datetime microseconds"
                                    else
                                      Ok 0
                                  in
                                  match read_micros with
                                  | Error _ as error -> error
                                  | Ok microsecond ->
                                      match column_type with
                                      | ColumnType.Date -> Ok (Value.date year month day)
                                      | _ ->
                                          Ok (
                                            Value.timestamp
                                              (
                                                DateTime.from_naive
                                                  DateTime.{
                                                    year;
                                                    month;
                                                    day;
                                                    hour;
                                                    minute;
                                                    second;
                                                    microsecond;
                                                  }
                                                  ~tz:DateTime.Tz.Etc_UTC
                                              )
                                          )
                                )
                            )
                        )
              )
          )
      )
    | Ok length -> Error (InvalidPayload ("invalid binary date length: " ^ Int.to_string length))

  let decode_binary_time = fun cursor ->
    match read_u8 cursor "binary time length" with
    | Error _ as error -> error
    | Ok 0 -> Ok (Value.time 0 0 0 0)
    | Ok length when length = 8 || length = 12 -> (
        match read_u8 cursor "binary time sign" with
        | Error _ as error -> error
        | Ok _sign -> (
            match read_int32_le cursor "binary time days" with
            | Error _ as error -> error
            | Ok days -> (
                match read_u8 cursor "binary time hour" with
                | Error _ as error -> error
                | Ok hour -> (
                    match read_u8 cursor "binary time minute" with
                    | Error _ as error -> error
                    | Ok minute -> (
                        match read_u8 cursor "binary time second" with
                        | Error _ as error -> error
                        | Ok second ->
                            let read_micros =
                              if length = 12 then
                                read_int32_le cursor "binary time microseconds"
                              else
                                Ok 0
                            in
                            match read_micros with
                            | Error _ as error -> error
                            | Ok microsecond ->
                                Ok (Value.time (days * 24 + hour) minute second microsecond)
                      )
                  )
              )
          )
      )
    | Ok length -> Error (InvalidPayload ("invalid binary time length: " ^ Int.to_string length))

  let read_float32 = fun cursor context ->
    match read_int32_le cursor context with
    | Error _ as error -> error
    | Ok bits -> Ok (Int32.float_of_bits (Int32.from_int bits))

  let read_float64 = fun cursor context ->
    match read_int64_le cursor context with
    | Error _ as error -> error
    | Ok bits -> Ok (Int64.float_of_bits bits)

  let unsigned_flag = 0x20

  let column_is_unsigned = fun column -> column.flags land unsigned_flag != 0

  let sign_extend_int = fun value bits ->
    let sign_bit = 1 lsl (bits - 1) in
    if value land sign_bit = 0 then
      value
    else
      value - (1 lsl bits)

  let uint64_div10_rem = fun value ->
    let quotient = ref 0L in
    let remainder = ref 0 in
    for bit_index = 63 downto 0 do
      let bit =
        Int64.shift_right_logical value bit_index
        |> Int64.logand 1L
        |> Int64.to_int
      in
      remainder := (!remainder * 2) + bit;
      if !remainder >= 10 then (
        remainder := !remainder - 10;
        quotient := Int64.logor !quotient (Int64.shift_left 1L bit_index)
      )
    done;
    (!quotient, !remainder)

  let uint64_to_decimal = fun value ->
    if Int64.compare value 0L != Order.LT then
      Int64.to_string value
    else
      let rec loop value digits =
        if Int64.equal value 0L then
          digits
        else
          let (quotient, remainder) = uint64_div10_rem value in
          loop quotient (Char.from_int_unchecked (Char.code '0' + remainder) :: digits)
      in
      loop value []
      |> fun digits ->
        let buffer = Buffer.create ~size:(List.length digits) in
        List.for_each digits ~fn:(Buffer.add_char buffer);
    Buffer.contents buffer

  let decode_binary_value = fun column cursor ->
    match column.column_type with
    | ColumnType.Tiny -> (
        match read_u8 cursor "binary tiny" with
        | Ok value ->
            let value =
              if column_is_unsigned column then
                value
              else
                sign_extend_int value 8
            in
            Ok (Value.int value)
        | Error _ as error -> error
      )
    | ColumnType.Short
    | ColumnType.Year -> (
        match read_int16_le cursor "binary short" with
        | Ok value ->
            let value =
              if column_is_unsigned column then
                value
              else
                sign_extend_int value 16
            in
            Ok (Value.int value)
        | Error _ as error -> error
      )
    | ColumnType.Long
    | ColumnType.Int24 -> (
        match read_int32_le cursor "binary long" with
        | Ok value ->
            let value =
              if column_is_unsigned column then
                value
              else
                sign_extend_int value 32
            in
            Ok (Value.int value)
        | Error _ as error -> error
      )
    | ColumnType.LongLong -> (
        match read_int64_le cursor "binary longlong" with
        | Ok value ->
            if column_is_unsigned column && Int64.compare value 0L = Order.LT then
              Ok (Value.numeric (uint64_to_decimal value))
            else
              Ok (Value.int64 value)
        | Error _ as error -> error
      )
    | ColumnType.Float -> (
        match read_float32 cursor "binary float" with
        | Ok value -> Ok (Value.float value)
        | Error _ as error -> error
      )
    | ColumnType.Double -> (
        match read_float64 cursor "binary double" with
        | Ok value -> Ok (Value.float value)
        | Error _ as error -> error
      )
    | ColumnType.NewDecimal -> (
        match read_lenenc_string cursor "binary decimal" with
        | Ok value -> Ok (Value.numeric value)
        | Error _ as error -> error
      )
    | ColumnType.Json -> (
        match read_lenenc_string cursor "binary json" with
        | Ok value -> Ok (Value.json value)
        | Error _ as error -> error
      )
    | ColumnType.Date
    | ColumnType.DateTime
    | ColumnType.Timestamp -> decode_binary_datetime column.column_type cursor
    | ColumnType.Time -> decode_binary_time cursor
    | ColumnType.TinyBlob
    | ColumnType.MediumBlob
    | ColumnType.LongBlob
    | ColumnType.Blob
    | ColumnType.Geometry -> (
        match read_lenenc_string cursor "binary bytes" with
        | Ok value -> Ok (Value.bytes (Bytes.from_string value))
        | Error _ as error -> error
      )
    | ColumnType.VarChar
    | ColumnType.Enum
    | ColumnType.Set
    | ColumnType.VarString
    | ColumnType.String
    | ColumnType.Unknown _ -> (
        match read_lenenc_string cursor "binary string" with
        | Ok value -> Ok (Value.string value)
        | Error _ as error -> error
      )
    | ColumnType.Null -> Ok Value.null

  let bitmap_is_null = fun bitmap index ->
    let bit = index + 2 in
    let byte_index = bit / 8 in
    let bit_index = bit mod 8 in
    byte_index < String.length bitmap && byte_at bitmap byte_index land (1 lsl bit_index) != 0

  let parse_binary_row = fun columns payload ->
    let cursor = cursor payload in
    match read_u8 cursor "binary row marker" with
    | Error _ as error -> error
    | Ok marker when marker != 0x00 -> Error (UnknownPacket marker)
    | Ok _ -> (
        let bitmap_length = (List.length columns + 9) / 8 in
        match read_bytes cursor bitmap_length "binary null bitmap" with
        | Error _ as error -> error
        | Ok bitmap ->
            let rec loop index columns acc =
              match columns with
              | [] ->
                  if remaining cursor = 0 then
                    Ok (List.rev acc)
                  else
                    Error (InvalidPayload "binary row has trailing bytes")
              | column :: rest ->
                  if bitmap_is_null bitmap index then
                    loop (index + 1) rest ((column.name, Value.null) :: acc)
                  else
                    match decode_binary_value column cursor with
                    | Error _ as error -> error
                    | Ok value -> loop (index + 1) rest ((column.name, value) :: acc)
            in
            loop 0 columns []
      )
end

module Writer = struct
  let add_lenenc_int = fun buffer value ->
    if Int64.compare value 251L = Order.LT then
      add_u8 buffer (Int64.to_int value)
    else if Int64.compare value 65_536L = Order.LT then (
      add_u8 buffer 0xfc;
      add_int16_le buffer (Int64.to_int value)
    ) else if Int64.compare value 16_777_216L = Order.LT then (
      add_u8 buffer 0xfd;
      add_int24_le buffer (Int64.to_int value)
    ) else (
      add_u8 buffer 0xfe;
      add_int64_le buffer value
    )

  let add_lenenc_string = fun buffer value ->
    add_lenenc_int buffer (Int64.from_int (String.length value));
    Buffer.add_string buffer value

  let packet = fun ~sequence ~payload -> Packet.encode (Packet.make ~sequence ~payload)

  let ssl_request = fun ~capability_flags ~max_packet_size ~character_set ->
    let buffer = Buffer.create ~size:32 in
    add_int32_le buffer capability_flags;
    add_int32_le buffer max_packet_size;
    add_u8 buffer character_set;
    Buffer.add_string buffer (String.make ~len:23 ~char:'\x00');
    Buffer.contents buffer

  let handshake_response = fun
    ~capability_flags ~max_packet_size ~character_set ~user ~database ~auth_response ~auth_plugin ->
    let buffer = Buffer.create ~size:256 in
    add_int32_le buffer capability_flags;
    add_int32_le buffer max_packet_size;
    add_u8 buffer character_set;
    Buffer.add_string buffer (String.make ~len:23 ~char:'\x00');
    add_cstring buffer user;
    add_lenenc_string buffer auth_response;
    (
      match database with
      | Some database -> add_cstring buffer database
      | None -> ()
    );
    add_cstring buffer auth_plugin;
    Buffer.contents buffer

  let com_query = fun sql ->
    let buffer = Buffer.create ~size:(String.length sql + 1) in
    add_u8 buffer 0x03;
    Buffer.add_string buffer sql;
    Buffer.contents buffer

  let com_stmt_prepare = fun sql ->
    let buffer = Buffer.create ~size:(String.length sql + 1) in
    add_u8 buffer 0x16;
    Buffer.add_string buffer sql;
    Buffer.contents buffer

  let add_null_bitmap = fun buffer params ->
    let len = (List.length params + 7) / 8 in
    let bytes = Bytes.create ~size:len in
    List.enumerate params
    |> List.for_each
      ~fn:(fun (index, value) ->
        match value with
        | Value.Null ->
            let byte_index = index / 8 in
            let bit_index = index mod 8 in
            let current = Char.code (Bytes.get_unchecked bytes ~at:byte_index) in
            Bytes.set_unchecked
              bytes
              ~at:byte_index
              ~char:(Char.from_int_unchecked (current lor (1 lsl bit_index)))
        | _ -> ());
    Buffer.add_string buffer (Bytes.to_string bytes)

  let parameter_type = fun value ->
    match value with
    | Value.Null -> ColumnType.Null
    | Value.Int _ -> ColumnType.Long
    | Value.Int64 _ -> ColumnType.LongLong
    | Value.Int16 _ -> ColumnType.Short
    | Value.Float _ -> ColumnType.Double
    | Value.String _ -> ColumnType.VarString
    | Value.Bool _ -> ColumnType.Tiny
    | Value.Bytes _ -> ColumnType.Blob
    | Value.Timestamp _
    | Value.TimestampWithTimezone _ -> ColumnType.DateTime
    | Value.Date _ -> ColumnType.Date
    | Value.Time _ -> ColumnType.Time
    | Value.Uuid _ -> ColumnType.VarString
    | Value.Json _ -> ColumnType.Json
    | Value.Numeric _ -> ColumnType.NewDecimal

  let add_parameter_type = fun buffer value ->
    add_u8 buffer (ColumnType.to_int (parameter_type value));
    add_u8 buffer 0

  let add_date = fun buffer year month day ->
    add_u8 buffer 4;
    add_int16_le buffer year;
    add_u8 buffer month;
    add_u8 buffer day

  let add_datetime = fun buffer datetime ->
    let (micros, _precision) = DateTime.(datetime.microseconds) in
    add_u8 buffer 11;
    add_int16_le buffer DateTime.(datetime.year);
    add_u8 buffer DateTime.(datetime.month);
    add_u8 buffer DateTime.(datetime.day);
    add_u8 buffer DateTime.(datetime.hour);
    add_u8 buffer DateTime.(datetime.minute);
    add_u8 buffer DateTime.(datetime.second);
    add_int32_le buffer micros

  let add_time = fun buffer hour minute second micros ->
    add_u8 buffer 12;
    add_u8 buffer 0;
    add_int32_le buffer (hour / 24);
    add_u8 buffer (hour mod 24);
    add_u8 buffer minute;
    add_u8 buffer second;
    add_int32_le buffer micros

  let add_parameter_value = fun buffer value ->
    match value with
    | Value.Null -> ()
    | Value.Int value -> add_int32_le buffer value
    | Value.Int64 value -> add_int64_le buffer value
    | Value.Int16 value -> add_int16_le buffer value
    | Value.Float value -> add_int64_le buffer (Int64.bits_of_float value)
    | Value.String value -> add_lenenc_string buffer value
    | Value.Bool value ->
        add_u8
          buffer
          (
            if value then
              1
            else
              0
          )
    | Value.Bytes value -> add_lenenc_string buffer (Bytes.to_string value)
    | Value.Timestamp value
    | Value.TimestampWithTimezone value -> add_datetime buffer value
    | Value.Date (year, month, day) -> add_date buffer year month day
    | Value.Time (hour, minute, second, micros) -> add_time buffer hour minute second micros
    | Value.Uuid value -> add_lenenc_string buffer value
    | Value.Json value -> add_lenenc_string buffer value
    | Value.Numeric value -> add_lenenc_string buffer value

  let com_stmt_execute = fun ~statement_id ~params ->
    let buffer = Buffer.create ~size:128 in
    add_u8 buffer 0x17;
    add_int32_le buffer statement_id;
    add_u8 buffer 0;
    add_int32_le buffer 1;
    if not (List.is_empty params) then (
      add_null_bitmap buffer params;
      add_u8 buffer 1;
      List.for_each params ~fn:(add_parameter_type buffer);
      List.for_each params ~fn:(add_parameter_value buffer)
    );
    Buffer.contents buffer

  let com_stmt_close = fun statement_id ->
    let buffer = Buffer.create ~size:5 in
    add_u8 buffer 0x19;
    add_int32_le buffer statement_id;
    Buffer.contents buffer

  let com_quit = fun () -> String.make ~len:1 ~char:(Char.from_int_unchecked 0x01)
end
