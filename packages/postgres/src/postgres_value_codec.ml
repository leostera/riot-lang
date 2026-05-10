open Std
open Std.IO

let strip_timezone_name = fun str ->
  match String.last_index str ' ' with
  | Some idx when idx > 10 -> (
      let after_space = String.sub str ~offset:(idx + 1) ~len:(String.length str - idx - 1) in
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

let parse_date = fun str ->
  match DateTime.parse (str ^ "T00:00:00Z") with
  | Ok dt -> Ok (dt.year, dt.month, dt.day)
  | Error _ -> Error ()

let parse_time = fun str ->
  match DateTime.parse ("1970-01-01T" ^ str ^ "Z") with
  | Ok dt ->
      let (micros, _) = dt.microseconds in
      Ok (dt.hour, dt.minute, dt.second, micros)
  | Error _ -> Error ()

let parse_timestamp = fun str ->
  let iso_str =
    match String.index_of str ~char:' ' with
    | Some idx ->
        let before = String.sub str ~offset:0 ~len:idx in
        let after = String.sub str ~offset:(idx + 1) ~len:(String.length str - idx - 1) in
        before ^ "T" ^ after ^ "Z"
    | None -> str ^ "Z"
  in
  DateTime.parse iso_str

let decode_bytea_hex = fun value ->
  if not (String.starts_with ~prefix:"\\x" value) then
    None
  else
    let hex = String.sub value ~offset:2 ~len:(String.length value - 2) in
    match Encoding.Base16.decode_bytes hex with
    | Ok bytes -> Some bytes
    | Error _ -> None

let encode_bytea_hex = fun bytes -> "\\x" ^ Encoding.Base16.encode_bytes_lower bytes

let is_ascii_digit = fun ch -> ch >= '0' && ch <= '9'

let normalize_short_timezone_offset = fun str ->
  let len = String.length str in
  if len >= 3 then
    let sign = String.get_unchecked str ~at:(len - 3) in
    let hour0 = String.get_unchecked str ~at:(len - 2) in
    let hour1 = String.get_unchecked str ~at:(len - 1) in
    if (sign = '+' || sign = '-') && is_ascii_digit hour0 && is_ascii_digit hour1 then
      str ^ ":00"
    else
      str
  else
    str

let parse_timestamptz = fun str ->
  let str =
    strip_timezone_name str
    |> normalize_short_timezone_offset
  in
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
  | Protocol.TypeOid.Date -> (
      match parse_date value with
      | Ok (year, month, day) -> Sqlx_driver.Value.date year month day
      | Error _ -> Sqlx_driver.Value.string value
    )
  | Protocol.TypeOid.Time -> (
      match parse_time value with
      | Ok (hour, minute, second, micros) -> Sqlx_driver.Value.time hour minute second micros
      | Error _ -> Sqlx_driver.Value.string value
    )
  | Protocol.TypeOid.Timestamp -> (
      match parse_timestamp value with
      | Ok dt -> Sqlx_driver.Value.timestamp dt
      | Error _ -> Sqlx_driver.Value.string value
    )
  | Protocol.TypeOid.Timestamptz -> (
      match parse_timestamptz value with
      | Ok dt -> Sqlx_driver.Value.timestamp_with_timezone dt
      | Error _ -> Sqlx_driver.Value.string value
    )
  | Protocol.TypeOid.Text
  | Protocol.TypeOid.Varchar
  | Protocol.TypeOid.Char -> Sqlx_driver.Value.string value
  | Protocol.TypeOid.Bytea -> (
      match decode_bytea_hex value with
      | Some bytes -> Sqlx_driver.Value.bytes bytes
      | None -> Sqlx_driver.Value.string value
    )
  | Protocol.TypeOid.Oid
  | Protocol.TypeOid.Interval
  | Protocol.TypeOid.Unknown _ -> Sqlx_driver.Value.string value

let pad = fun n width ->
  let s = string_of_int n in
  String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s

let datetime_to_pg_format = fun dt ->
  let (micros, _precision) = dt.DateTime.microseconds in
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
  | Sqlx_driver.Value.Null -> None
  | Sqlx_driver.Value.Int n -> Some (string_of_int n)
  | Sqlx_driver.Value.Int64 n -> Some (Int64.to_string n)
  | Sqlx_driver.Value.Int16 n -> Some (string_of_int n)
  | Sqlx_driver.Value.Float f -> Some (string_of_float f)
  | Sqlx_driver.Value.String s -> Some s
  | Sqlx_driver.Value.Bool true -> Some "t"
  | Sqlx_driver.Value.Bool false -> Some "f"
  | Sqlx_driver.Value.Bytes b -> Some (encode_bytea_hex b)
  | Sqlx_driver.Value.Timestamp dt -> Some (datetime_to_pg_format dt)
  | Sqlx_driver.Value.TimestampWithTimezone dt -> Some (datetime_to_pg_format dt)
  | Sqlx_driver.Value.Date (y, m, d) -> Some (pad y 4 ^ "-" ^ pad m 2 ^ "-" ^ pad d 2)
  | Sqlx_driver.Value.Time (h, min, s, us) ->
      Some (pad h 2 ^ ":" ^ pad min 2 ^ ":" ^ pad s 2 ^ "." ^ pad us 6)
  | Sqlx_driver.Value.Uuid u -> Some u
  | Sqlx_driver.Value.Json j -> Some j
  | Sqlx_driver.Value.Numeric n -> Some n
