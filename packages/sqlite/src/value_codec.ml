open Std
open Result.Syntax

module Error = Sqlite__Error
module Native = Sqlite__Native
module Value = Sqlx_driver.Value

let pad_int = fun value width ->
  let raw = Int.to_string value in
  String.make ~len:(max 0 (width - String.length raw)) ~char:'0' ^ raw

let date_to_text = fun year month day ->
  pad_int year 4 ^ "-" ^ pad_int month 2 ^ "-" ^ pad_int day 2

let time_to_text = fun hour minute second micros ->
  pad_int hour 2 ^ ":" ^ pad_int minute 2 ^ ":" ^ pad_int second 2 ^ "." ^ pad_int micros 6

let text_of_value = fun value ->
  match value with
  | Value.String text
  | Value.Uuid text
  | Value.Json text
  | Value.Numeric text -> text
  | Value.Timestamp dt
  | Value.TimestampWithTimezone dt -> DateTime.to_iso8601 dt
  | Value.Date (year, month, day) -> date_to_text year month day
  | Value.Time (hour, minute, second, micros) -> time_to_text hour minute second micros
  | _ -> Value.to_string value

let bind_param = fun stmt index value ->
  let bind_result =
    match value with
    | Value.Null -> Native.bind_null stmt index
    | Value.Int n
    | Value.Int16 n -> Native.bind_int64 stmt index (Int64.from_int n)
    | Value.Int64 n -> Native.bind_int64 stmt index n
    | Value.Bool value ->
        Native.bind_int64
          stmt
          index
          (
            if value then
              1L
            else
              0L
          )
    | Value.Float n -> Native.bind_double stmt index n
    | Value.Bytes bytes -> Native.bind_blob stmt index bytes
    | Value.String _
    | Value.Timestamp _
    | Value.TimestampWithTimezone _
    | Value.Date _
    | Value.Time _
    | Value.Uuid _
    | Value.Json _
    | Value.Numeric _ -> Native.bind_text stmt index (text_of_value value)
  in
  Result.map_err bind_result ~fn:(fun cause -> Error.BindFailed { index; cause })

let bind_params = fun stmt params ->
  let expected = Native.bind_parameter_count stmt in
  let actual = List.length params in
  if expected != actual then
    Error (Error.ParameterCountMismatch { expected; actual })
  else
    let rec loop index values =
      match values with
      | [] -> Ok ()
      | value :: rest ->
          let* () = bind_param stmt index value in
          loop (index + 1) rest
    in
    loop 1 params

let decode_column = fun stmt index ->
  match Native.column_type stmt index with
  | value when value = Native.sqlite_null -> Value.null
  | value when value = Native.sqlite_integer ->
      let raw = Native.column_int64 stmt index in
      if
        Int64.compare raw (Int64.from_int Int.min_int) != Order.LT
        && Int64.compare raw (Int64.from_int Int.max_int) != Order.GT
      then
        Value.int (Int64.to_int raw)
      else
        Value.int64 raw
  | value when value = Native.sqlite_float -> Value.float (Native.column_double stmt index)
  | value when value = Native.sqlite_text -> Value.string (Native.column_text stmt index)
  | value when value = Native.sqlite_blob -> Value.bytes (Native.column_blob stmt index)
  | _ -> Value.null

let read_row = fun stmt ->
  let count = Native.column_count stmt in
  List.init
    ~count
    ~fn:(fun index ->
      let name = Native.column_name stmt index in
      let name =
        if String.length name = 0 then
          "col_" ^ Int.to_string index
        else
          name
      in
      (name, decode_column stmt index))
