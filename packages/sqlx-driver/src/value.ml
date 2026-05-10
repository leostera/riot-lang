open Std
open Std.IO

type t =
  | Null
  | Int of int
  | Int64 of int64
  | Int16 of int
  | Float of float
  | String of string
  | Bool of bool
  | Bytes of bytes
  | Timestamp of DateTime.t
  | TimestampWithTimezone of DateTime.t
  | Date of int * int * int
  | Time of int * int * int * int
  | Uuid of string
  | Json of string
  | Numeric of string

let null = Null

let int = fun n -> Int n

let int64 = fun n -> Int64 n

let int16 = fun n -> Int16 n

let string = fun s -> String s

let bool = fun b -> Bool b

let float = fun f -> Float f

let bytes = fun b -> Bytes b

let timestamp = fun dt -> Timestamp dt

let timestamp_with_timezone = fun dt -> TimestampWithTimezone dt

let date = fun y m d -> Date (y, m, d)

let time = fun h min s us -> Time (h, min, s, us)

let uuid = fun s -> Uuid s

let json = fun s -> Json s

let numeric = fun s -> Numeric s

let to_int = fun value ->
  match value with
  | Int n -> Some n
  | _ -> None

let bytes_equal = fun left right -> String.equal (Bytes.to_string left) (Bytes.to_string right)

let bytes_compare = fun left right -> String.compare (Bytes.to_string left) (Bytes.to_string right)

let to_int64 = fun value ->
  match value with
  | Int64 n -> Some n
  | _ -> None

let to_int16 = fun value ->
  match value with
  | Int16 n -> Some n
  | _ -> None

let to_string_value = fun value ->
  match value with
  | String s -> Some s
  | _ -> None

let to_bool = fun value ->
  match value with
  | Bool b -> Some b
  | _ -> None

let to_float = fun value ->
  match value with
  | Float f -> Some f
  | _ -> None

let to_bytes = fun value ->
  match value with
  | Bytes b -> Some b
  | _ -> None

let to_timestamp = fun value ->
  match value with
  | Timestamp dt -> Some dt
  | _ -> None

let to_timestamp_with_timezone = fun value ->
  match value with
  | TimestampWithTimezone dt -> Some dt
  | _ -> None

let to_date = fun value ->
  match value with
  | Date (y, m, d) -> Some (y, m, d)
  | _ -> None

let to_time = fun value ->
  match value with
  | Time (h, m, s, us) -> Some (h, m, s, us)
  | _ -> None

let to_uuid = fun value ->
  match value with
  | Uuid s -> Some s
  | _ -> None

let to_json = fun value ->
  match value with
  | Json s -> Some s
  | _ -> None

let to_numeric = fun value ->
  match value with
  | Numeric s -> Some s
  | _ -> None

let is_null = fun value ->
  match value with
  | Null -> true
  | _ -> false

let to_string = fun value ->
  match value with
  | Null -> "NULL"
  | Int n -> string_of_int n
  | Int64 n -> Int64.to_string n
  | Int16 n -> string_of_int n
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Bool b -> Bool.to_string b
  | Bytes b -> "<bytes:" ^ string_of_int (Bytes.length b) ^ ">"
  | Timestamp dt -> DateTime.to_iso8601 dt
  | TimestampWithTimezone dt -> DateTime.to_iso8601 dt
  | Date (y, m, d) ->
      let pad n width =
        let s = string_of_int n in
        String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
      in
      pad y 4 ^ "-" ^ pad m 2 ^ "-" ^ pad d 2
  | Time (h, min, s, us) ->
      let pad n width =
        let s = string_of_int n in
        String.make ~len:(max 0 (width - String.length s)) ~char:'0' ^ s
      in
      pad h 2 ^ ":" ^ pad min 2 ^ ":" ^ pad s 2 ^ "." ^ pad us 6
  | Uuid s -> s
  | Json s -> s
  | Numeric s -> s

let equal = fun a b ->
  match (a, b) with
  | (Null, Null) -> true
  | (Int x, Int y) -> x = y
  | (Int64 x, Int64 y) -> Int64.equal x y
  | (Int16 x, Int16 y) -> x = y
  | (Float x, Float y) -> x = y
  | (String x, String y) -> x = y
  | (Bool x, Bool y) -> x = y
  | (Bytes x, Bytes y) -> bytes_equal x y
  | (Timestamp x, Timestamp y) -> DateTime.equal x y
  | (TimestampWithTimezone x, TimestampWithTimezone y) -> DateTime.equal x y
  | (Date (y1, m1, d1), Date (y2, m2, d2)) -> y1 = y2 && m1 = m2 && d1 = d2
  | (Time (h1, min1, s1, us1), Time (h2, min2, s2, us2)) ->
      h1 = h2 && min1 = min2 && s1 = s2 && us1 = us2
  | (Uuid x, Uuid y) -> x = y
  | (Json x, Json y) -> x = y
  | (Numeric x, Numeric y) -> x = y
  | _ -> false

let compare = fun a b ->
  match (a, b) with
  | (Null, Null) -> Order.EQ
  | (Null, _) -> Order.LT
  | (_, Null) -> Order.GT
  | (Int x, Int y) -> Int.compare x y
  | (Int64 x, Int64 y) -> Int64.compare x y
  | (Int16 x, Int16 y) -> Int.compare x y
  | (Float x, Float y) -> Float.compare x y
  | (String x, String y) -> String.compare x y
  | (Bool x, Bool y) -> Bool.compare x y
  | (Bytes x, Bytes y) -> bytes_compare x y
  | (Timestamp x, Timestamp y) ->
      Time.SystemTime.compare (DateTime.to_system_time x) (DateTime.to_system_time y)
  | (TimestampWithTimezone x, TimestampWithTimezone y) ->
      Time.SystemTime.compare (DateTime.to_system_time x) (DateTime.to_system_time y)
  | (Date (y1, m1, d1), Date (y2, m2, d2)) -> (
      match Int.compare y1 y2 with
      | Order.EQ -> (
          match Int.compare m1 m2 with
          | Order.EQ -> Int.compare d1 d2
          | c -> c
        )
      | c -> c
    )
  | (Time (h1, min1, s1, us1), Time (h2, min2, s2, us2)) -> (
      match Int.compare h1 h2 with
      | Order.EQ -> (
          match Int.compare min1 min2 with
          | Order.EQ -> (
              match Int.compare s1 s2 with
              | Order.EQ -> Int.compare us1 us2
              | c -> c
            )
          | c -> c
        )
      | c -> c
    )
  | (Uuid x, Uuid y) -> String.compare x y
  | (Json x, Json y) -> String.compare x y
  | (Numeric x, Numeric y) -> String.compare x y
  | (Int _, _) -> Order.LT
  | (_, Int _) -> Order.GT
  | (Int64 _, _) -> Order.LT
  | (_, Int64 _) -> Order.GT
  | (Int16 _, _) -> Order.LT
  | (_, Int16 _) -> Order.GT
  | (Float _, _) -> Order.LT
  | (_, Float _) -> Order.GT
  | (String _, _) -> Order.LT
  | (_, String _) -> Order.GT
  | (Bool _, _) -> Order.LT
  | (_, Bool _) -> Order.GT
  | (Bytes _, _) -> Order.LT
  | (_, Bytes _) -> Order.GT
  | (Timestamp _, _) -> Order.LT
  | (_, Timestamp _) -> Order.GT
  | (TimestampWithTimezone _, _) -> Order.LT
  | (_, TimestampWithTimezone _) -> Order.GT
  | (Date _, _) -> Order.LT
  | (_, Date _) -> Order.GT
  | (Time _, _) -> Order.LT
  | (_, Time _) -> Order.GT
  | (Uuid _, _) -> Order.LT
  | (_, Uuid _) -> Order.GT
  | (Json _, _) -> Order.LT
  | (_, Json _) -> Order.GT
